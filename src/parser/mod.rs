use std::ops::Range;

use chumsky::{
    IterParser, ParseResult, Parser,
    error::Rich,
    extra::{Full, ParserExtra},
    input::{Input, MapExtra},
    pratt::{prefix, infix, left, postfix},
    prelude::{choice, just, recursive},
    span::SimpleSpan,
};

use logos::Logos;

use crate::{
    ast::{
        Arith, Cmp, Expr, FileId, Func, Literal, Logic, Module, Op, Path, Span, Stmt, Struct, Type,
        TypeKind, constructor_name,
    },
    parser::token::TokenKind,
};

pub mod token;

#[derive(Debug)]
pub struct BadToken(pub Range<usize>);

pub fn scan_program(text: &str) -> Result<Vec<(TokenKind, SimpleSpan)>, BadToken> {
    let token_iter = TokenKind::lexer(text);
    let mut tokens = Vec::new();
    for (token_kind, span) in token_iter.spanned() {
        tokens.push((
            token_kind.map_err(|_| BadToken(span.clone()))?,
            SimpleSpan::from(span),
        ));
    }
    Ok(tokens)
}

pub fn parse_program<'a>(
    text: &'a str,
    tokens: &'a [(TokenKind, SimpleSpan)],
    file_id: FileId,
) -> ParseResult<Module, Rich<'a, TokenKind>> {
    let eoi = SimpleSpan::from(text.len()..text.len());
    let input = tokens.split_token_span(eoi);

    let parser = program(text, file_id);
    parser.parse(input)
}

type Ctx<'a> = Full<Rich<'a, TokenKind>, (), ()>;

fn list<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>, T: Clone>(
    elem: impl Parser<'a, I, T, Ctx<'a>> + Clone,
    _file_id: FileId,
) -> impl Parser<'a, I, Vec<T>, Ctx<'a>> + Clone {
    elem.clone()
        .then(
            just(TokenKind::Comma)
                .ignore_then(elem)
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|(first, mut rest)| {
            rest.insert(0, first);
            rest
        })
        .or_not()
        .map(|list| list.unwrap_or_default())
}

fn get_span<'src, 'b, I: Input<'src, Span = SimpleSpan>, E: ParserExtra<'src, I>>(
    extra: &mut MapExtra<'src, 'b, I, E>,
    file_id: FileId,
) -> Span {
    let span: SimpleSpan = extra.span();
    Span::new(file_id, span.start, span.end)
}

fn program<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>>(
    input: &'a str,
    file_id: FileId,
) -> impl Parser<'a, I, Module, Ctx<'a>> {
    module(input, file_id)
}

fn module<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>>(
    input: &'a str,
    file_id: FileId,
) -> impl Parser<'a, I, Module, Ctx<'a>> {
    let typ = recursive(move |typ| {
        let path = name(input)
            .then_ignore(just(TokenKind::DoubleColon))
            .repeated()
            .collect()
            .then(upper_name(input));
        let named = path
            .then(
                list(typ.clone(), file_id)
                    .delimited_by(just(TokenKind::LeftSquare), just(TokenKind::RightSquare))
                    .or_not()
                    .map(|list| list.unwrap_or_default()),
            )
            .map_with(move |((path, name), children), e| Type {
                kind: TypeKind::Named(Path::new(path, get_span(e, file_id)), name),
                span: get_span(e, file_id),
                children,
            });
        let generic = name(input)
            .map_with(move |name, e| Type::base(TypeKind::Generic(name, 0), get_span(e, file_id)));
        let slice = just(TokenKind::LeftSquare)
            .ignore_then(just(TokenKind::RightSquare))
            .ignore_then(typ.clone())
            .map_with(move |elem, e| Type::slice(elem, get_span(e, file_id)));
        let any = just(TokenKind::Any)
            .ignore_then(name(input).delimited_by(just(TokenKind::LeftSquare), just(TokenKind::RightSquare)))
            .then(typ.clone()).map_with(move |(var, typ), e| Type {
                kind: TypeKind::Any(var),
                children: vec![typ],
                span: get_span(e, file_id)
            });
        let func = just(TokenKind::Func)
            .ignore_then(list(name(input), file_id).delimited_by(just(TokenKind::LeftSquare), just(TokenKind::RightSquare)).or_not().map(|g| g.unwrap_or_default()))
            .then(list(typ.clone(), file_id).delimited_by(just(TokenKind::LeftParen), just(TokenKind::RightParen)))
            .then_ignore(just(TokenKind::Colon))
            .then(typ.clone())
            .map_with(move |((generics, args), result), e| Type::func(generics, args, result, get_span(e, file_id)));

        let base = generic.or(named).or(slice).or(func).or(any);
        base.then(just(TokenKind::Star).repeated().collect::<Vec<_>>()).map_with(move |(mut typ, stars), e| {
            for _star in stars {
                typ = Type::ptr(typ, get_span(e, file_id));
            }
            typ
        })
    });

    let expr = {
        let typ = typ.clone();
        recursive(move |expr| {
            let number =
                num(input).map_with(move |num, e| Literal::Number(num, get_span(e, file_id)));
            let _true = just(TokenKind::True)
                .map_with(move |_, e| Literal::Bool(true, get_span(e, file_id)));
            let _false = just(TokenKind::False)
                .map_with(move |_, e| Literal::Bool(false, get_span(e, file_id)));
            let literal = number
                .or(_true)
                .or(_false)
                .map(|lit| Expr::Literal(lit, None));
            let unqualified_constructor = upper_name(input).map_with(move |name, e| {
                Expr::Func(
                    Path::new(vec![], get_span(e, file_id)),
                    name,
                    None,
                    get_span(e, file_id),
                )
            });
            let pathed_callable = name(input)
                .then(
                    just(TokenKind::DoubleColon)
                        .ignore_then(name(input))
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then(
                    just(TokenKind::DoubleColon)
                        .ignore_then(upper_name(input))
                        .or_not(),
                )
                .map_with(move |((prefix, mut path), end), e| {
                    if path.is_empty() && end == None {
                        Expr::Var(prefix, None, get_span(e, file_id))
                    } else {
                        path.insert(0, prefix);
                        if let Some(func_name) = end {
                            let constructor = constructor_name(&func_name);
                            Expr::Func(
                                Path::new(path, get_span(e, file_id)),
                                constructor,
                                None,
                                get_span(e, file_id),
                            )
                        } else {
                            let func_name = path.pop().unwrap();
                            Expr::Func(
                                Path::new(path, get_span(e, file_id)),
                                func_name,
                                None,
                                get_span(e, file_id),
                            )
                        }
                    }
                });

            let array_literal = just(TokenKind::LeftSquare)
                .ignore_then(num(input).or_not())
                .then_ignore(just(TokenKind::RightSquare))
                .then(typ.clone().or_not())
                .then(
                    list(expr.clone(), file_id)
                        .delimited_by(just(TokenKind::LeftBrace), just(TokenKind::RightBrace)),
                )
                .map_with(move |((size, typ), elems), e| {
                    if let Some(size) = size {
                        let elems = if elems.is_empty() { None } else { Some(elems) };
                        Expr::Array(size.parse().unwrap(), elems, typ, get_span(e, file_id))
                    } else {
                        Expr::Array(elems.len(), Some(elems), typ, get_span(e, file_id))
                    }
                });

            let let_stmt = just(TokenKind::Let)
                .ignore_then(name(input))
                .then_ignore(just(TokenKind::Equals))
                .then(expr.clone())
                .map(|(var, val)| Stmt::Let(var, val));
            let set_stmt = just(TokenKind::Set)
                .ignore_then(expr.clone())
                .then_ignore(just(TokenKind::Equals))
                .then(expr.clone())
                .map(|(lval, val)| Stmt::Set(lval, val));
            let expr_stmt = expr.clone().map(Stmt::Expr);
            let stmt = let_stmt.or(set_stmt).or(expr_stmt);
            let block = just(TokenKind::LeftBrace)
                .ignore_then(
                    stmt.clone()
                        .then_ignore(just(TokenKind::Semicolon))
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then(stmt.or_not())
                .then_ignore(just(TokenKind::RightBrace))
                .map_with(move |(mut stmts, last_stmt), e| {
                    let span = get_span(e, file_id);
                    if let Some(stmt) = last_stmt {
                        if let Stmt::Expr(expr) = stmt {
                            Expr::Block(stmts, Some(Box::new(expr)), span)
                        } else {
                            stmts.push(stmt);
                            Expr::Block(stmts, None, span)
                        }
                    } else {
                        Expr::Block(stmts, None, span)
                    }
                });
            let _if = just(TokenKind::If)
                .ignore_then(expr.clone())
                .then(
                    just(TokenKind::Then)
                        .ignore_then(expr.clone())
                        .then_ignore(just(TokenKind::Else))
                        .then(expr.clone())
                        .or(block
                            .clone()
                            .then_ignore(just(TokenKind::Else))
                            .then(expr.clone())),
                )
                .map_with(move |(cond, (if_true, if_false)), e| {
                    Expr::Op(
                        Op::If,
                        vec![cond, if_true, if_false],
                        None,
                        get_span(e, file_id),
                    )
                });
            let _while = just(TokenKind::While)
                .ignore_then(expr.clone())
                .then(block.clone())
                .map_with(move |(cond, body), e| {
                    Expr::Op(Op::While, vec![cond, body], None, get_span(e, file_id))
                });
            let parens = expr
                .clone()
                .delimited_by(just(TokenKind::LeftParen), just(TokenKind::RightParen));
            let _yield = just(TokenKind::Yield)
                .map_with(move |_, e| Expr::Op(Op::Yield, vec![], None, get_span(e, file_id)));
            let base = literal
                .or(unqualified_constructor)
                .or(pathed_callable)
                .or(block)
                .or(_if)
                .or(_while)
                .or(_yield)
                .or(array_literal)
                .or(parens);

            let call_args = list(expr.clone(), file_id)
                .delimited_by(just(TokenKind::LeftParen), just(TokenKind::RightParen));
            let index = expr
                .clone()
                .delimited_by(just(TokenKind::LeftSquare), just(TokenKind::RightSquare));
            let atom_start = choice((
                just(TokenKind::True),
                just(TokenKind::False),
                just(TokenKind::Yield),
                just(TokenKind::Name),
                just(TokenKind::UpperName),
                just(TokenKind::LeftParen),
                just(TokenKind::LeftBrace),
                just(TokenKind::If),
                just(TokenKind::While),
                just(TokenKind::LeftSquare),
                just(TokenKind::Number),
            ));
            let mul_op =
                just(TokenKind::Star).and_is(just(TokenKind::Star).then(atom_start.clone()));
            let deref_op =
                just(TokenKind::Star).and_is(just(TokenKind::Star).then(atom_start.clone().not()));
            let ref_op = just(TokenKind::Ampersand)
                .and_is(just(TokenKind::Ampersand).then(atom_start.clone().not()));
            let bit_and_op = just(TokenKind::Ampersand)
                .and_is(just(TokenKind::Ampersand).then(atom_start.clone()));
            base.pratt((
                prefix(
                    101,
                    just(TokenKind::Open),
                    move |_, inner, e| Expr::Op(Op::Open(None), vec![inner], None, get_span(e, file_id))
                ),
                postfix(
                    100,
                    just(TokenKind::Dot).ignore_then(name(input)),
                    move |lhs, name, e| {
                        Expr::Field(Box::new(lhs), name, None, get_span(e, file_id))
                    },
                ),
                postfix(99, call_args.clone(), move |lhs, args, e| {
                    Expr::Call(Box::new(lhs), args, None, get_span(e, file_id))
                }),
                postfix(98, just(TokenKind::Bang), move |lhs, _, e| {
                    Expr::Op(Op::Await, vec![lhs], None, get_span(e, file_id))
                }),
                postfix(97, deref_op, move |lhs, _, e| {
                    Expr::Op(Op::Deref, vec![lhs], None, get_span(e, file_id))
                }),
                postfix(96, ref_op, move |lhs, _, e| {
                    Expr::Op(Op::Ref, vec![lhs], None, get_span(e, file_id))
                }),
                postfix(95, index, move |lhs, index, e| {
                    Expr::Op(Op::SliceIndex, vec![lhs, index], None, get_span(e, file_id))
                }),
                infix(left(94), mul_op, move |lhs, _, rhs, e| {
                    Expr::Op(
                        Op::Arith(Arith::Mul),
                        vec![lhs, rhs],
                        None,
                        get_span(e, file_id),
                    )
                }),
                infix(left(94), just(TokenKind::Slash), move |lhs, _, rhs, e| {
                    Expr::Op(
                        Op::Arith(Arith::Div),
                        vec![lhs, rhs],
                        None,
                        get_span(e, file_id),
                    )
                }),
                infix(left(93), just(TokenKind::Plus), move |lhs, _, rhs, e| {
                    Expr::Op(
                        Op::Arith(Arith::Add),
                        vec![lhs, rhs],
                        None,
                        get_span(e, file_id),
                    )
                }),
                infix(left(93), just(TokenKind::Dash), move |lhs, _, rhs, e| {
                    Expr::Op(
                        Op::Arith(Arith::Sub),
                        vec![lhs, rhs],
                        None,
                        get_span(e, file_id),
                    )
                }),
                infix(
                    left(92),
                    just(TokenKind::ShiftLeft),
                    move |lhs, _, rhs, e| {
                        Expr::Op(
                            Op::Arith(Arith::ShiftLeft),
                            vec![lhs, rhs],
                            None,
                            get_span(e, file_id),
                        )
                    },
                ),
                infix(
                    left(92),
                    just(TokenKind::ShiftRight),
                    move |lhs, _, rhs, e| {
                        Expr::Op(
                            Op::Arith(Arith::ShiftRight),
                            vec![lhs, rhs],
                            None,
                            get_span(e, file_id),
                        )
                    },
                ),
                infix(
                    left(91),
                    just(TokenKind::GreaterThan),
                    move |lhs, _, rhs, e| {
                        Expr::Op(Op::Cmp(Cmp::Gt), vec![lhs, rhs], None, get_span(e, file_id))
                    },
                ),
                infix(
                    left(91),
                    just(TokenKind::GreaterEqual),
                    move |lhs, _, rhs, e| {
                        Expr::Op(Op::Cmp(Cmp::Ge), vec![lhs, rhs], None, get_span(e, file_id))
                    },
                ),
                infix(
                    left(91),
                    just(TokenKind::LessThan),
                    move |lhs, _, rhs, e| {
                        Expr::Op(Op::Cmp(Cmp::Lt), vec![lhs, rhs], None, get_span(e, file_id))
                    },
                ),
                infix(
                    left(91),
                    just(TokenKind::LessEqual),
                    move |lhs, _, rhs, e| {
                        Expr::Op(Op::Cmp(Cmp::Le), vec![lhs, rhs], None, get_span(e, file_id))
                    },
                ),
                infix(
                    left(90),
                    just(TokenKind::DoubleEquals),
                    move |lhs, _, rhs, e| {
                        Expr::Op(Op::Cmp(Cmp::Eq), vec![lhs, rhs], None, get_span(e, file_id))
                    },
                ),
                infix(
                    left(90),
                    just(TokenKind::NotEquals),
                    move |lhs, _, rhs, e| {
                        Expr::Op(Op::Cmp(Cmp::Ne), vec![lhs, rhs], None, get_span(e, file_id))
                    },
                ),
                infix(left(85), bit_and_op, move |lhs, _, rhs, e| {
                    Expr::Op(
                        Op::Arith(Arith::BitAnd),
                        vec![lhs, rhs],
                        None,
                        get_span(e, file_id),
                    )
                }),
                infix(left(80), just(TokenKind::BitXor), move |lhs, _, rhs, e| {
                    Expr::Op(
                        Op::Arith(Arith::BitXor),
                        vec![lhs, rhs],
                        None,
                        get_span(e, file_id),
                    )
                }),
                infix(left(75), just(TokenKind::BitOr), move |lhs, _, rhs, e| {
                    Expr::Op(
                        Op::Arith(Arith::BitOr),
                        vec![lhs, rhs],
                        None,
                        get_span(e, file_id),
                    )
                }),
                infix(
                    left(70),
                    just(TokenKind::LogicAnd),
                    move |lhs, _, rhs, e| {
                        Expr::Op(
                            Op::Logic(Logic::And),
                            vec![lhs, rhs],
                            None,
                            get_span(e, file_id),
                        )
                    },
                ),
                infix(left(65), just(TokenKind::LogicOr), move |lhs, _, rhs, e| {
                    Expr::Op(
                        Op::Logic(Logic::Or),
                        vec![lhs, rhs],
                        None,
                        get_span(e, file_id),
                    )
                }),
            ))
        })
    };

    let generics = list(name(input), file_id)
        .delimited_by(just(TokenKind::LeftSquare), just(TokenKind::RightSquare))
        .or_not()
        .map(|generics| generics.unwrap_or_default());
    let field = name(input)
        .then_ignore(just(TokenKind::Colon))
        .then(typ.clone());
    let arg = name(input)
        .then_ignore(just(TokenKind::Colon))
        .then(typ.clone());
    let func = just(TokenKind::Func)
        .to(false)
        .or(just(TokenKind::Cor).to(true))
        .then(name(input))
        .then(generics.clone())
        .then(
            list(arg, file_id)
                .delimited_by(just(TokenKind::LeftParen), just(TokenKind::RightParen)),
        )
        .then_ignore(just(TokenKind::Colon))
        .then(typ.clone())
        .then_ignore(just(TokenKind::Equals))
        .then(expr)
        .map(
            |(((((is_cor, name), generics), args), result), body)| Func {
                name,
                generics,
                args,
                result,
                body,
                is_cor,
            },
        );
    let strukt = just(TokenKind::Struct)
        .ignore_then(upper_name(input))
        .then(generics)
        .then_ignore(just(TokenKind::LeftBrace))
        .then(field.repeated().collect::<Vec<_>>())
        .then_ignore(just(TokenKind::RightBrace))
        .map_with(move |((name, generics), fields), e| Struct {
            name,
            generics: generics,
            fields: fields.into_iter().collect(),
            span: get_span(e, file_id),
        });
    strukt
        .repeated()
        .collect::<Vec<_>>()
        .then(func.repeated().collect::<Vec<_>>())
        .map(|(structs, funcs)| Module {
            structs: structs.into_iter().map(|s| (s.name.clone(), s)).collect(),
            funcs: funcs.into_iter().map(|f| (f.name.clone(), f)).collect(),
        })
}

fn name<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>>(
    input: &'a str,
) -> impl Parser<'a, I, String, Ctx<'a>> + Clone {
    just(TokenKind::Name).map_with(|_, e| {
        let span: SimpleSpan = e.span();
        input[span.into_range()].to_string()
    })
}

fn upper_name<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>>(
    input: &'a str,
) -> impl Parser<'a, I, String, Ctx<'a>> + Clone {
    just(TokenKind::UpperName).map_with(|_, e| {
        let span: SimpleSpan = e.span();
        input[span.into_range()].to_string()
    })
}

fn num<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>>(
    input: &'a str,
) -> impl Parser<'a, I, String, Ctx<'a>> + Clone {
    just(TokenKind::Number).map_with(|_, e| {
        let span: SimpleSpan = e.span();
        input[span.into_range()].to_string()
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_parse(text: &str) {
        let tokens = scan_program(text).unwrap();
        let file_id = 0;
        let _ = parse_program(text, &tokens, file_id).unwrap();
    }

    #[test]
    fn empty_generic_struct() {
        let source = r#"
            struct Cat[t] {
            
            }
        "#;
        test_parse(source);
    }

    #[test]
    fn struct_with_field() {
        let source = r#"
            struct Cat[t] {
                inner: t
            }
        "#;
        test_parse(source);
    }

    #[test]
    fn weird_types() {
        let source = r#"
            struct Zoo[x, y] {
                zookeeper: x
                lion: Ptr[Cat[y]]
                escaped_animals: Int
            }
        "#;
        test_parse(source);
    }

    #[test]
    fn struct_with_func() {
        let source = "func main(): Int = 3";
        test_parse(source);
    }

    #[test]
    fn feed_cats() {
        let source = r#"
            struct Cat[t] {
                inner: t
            }

            func main(c: Cat[t]): Unit = cats::feed(c, 3)
        "#;
        test_parse(source);
    }

    #[test]
    fn cors() {
        let source = r#"
            cor foo(y: Int): Int = {
                yield;
                let x = y;
                yield;
                x
            }

            cor bar(x: Int): Int = {
                yield;
                let q = foo(int::add(x, 1))!;
                q
            }

            func poll_twice(sm: Ptr[Main_bar], result: Ptr[Int]): Unit = {
                bar::poll(sm, result);
                bar::poll(sm, result);
            }

            func main(): Int = {
                let sm = bar(3);
            }
        "#;
        test_parse(source);
    }

    #[test]
    fn blocks_loops_ifs() {
        let source = r#"
            func main(): Int = {
                let total = 0;
                let i = 0;
                while int::less_than(i, 10) {
                    if int::less_than(i, 5) {
                        set total = int::add(total, i);
                    } else {};
                    set i = int::add(i, 1);
                };
                total
            }
        "#;
        test_parse(source);
    }

    #[test]
    fn getters_ref() {
        let source = r#"
            struct Pair[x, y] {
                a: x
                b: y
            }
            func first(p: Pair[x, y]): x = p.a

            func f(p: Pair[Int, Int]): Int = {
                let x = p.a;
                ptr::store(x&, 7);
                x
            }
        "#;
        test_parse(source);
    }

    #[test]
    fn constructor() {
        let source = r#"
            struct Box[t] {
                inner: t
            }

            func main(): Unit = {
                let x = Box(3);
                let y = box::Box(3);
            }
        "#;
        test_parse(source);
    }

    #[test]
    fn any_open() {
        let source = r#"
            struct Task[t] {
                state: t*
                op: func(t): Unit
            }

            func run_task(task: any[t] Task[t]): Unit = {
                let inner = open task;
                (inner.op)(inner.state*)
            }
        "#;
        test_parse(source);
    }
}
