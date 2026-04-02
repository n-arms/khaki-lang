use std::ops::Range;

use chumsky::{
    IterParser, ParseResult, Parser,
    error::Rich,
    extra::{Full, ParserExtra},
    input::{Input, MapExtra},
    pratt::{infix, left, postfix},
    prelude::{choice, just, recursive},
    span::SimpleSpan,
};

use logos::Logos;

use crate::{
    ast::{
        Arith, Cmp, Expr, FileId, Func, FuncSpec, Literal, Logic, Op, Span, Stmt, Struct, Type,
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
) -> ParseResult<Vec<Struct>, Rich<'a, TokenKind>> {
    let eoi = SimpleSpan::from(text.len()..text.len());
    let input = tokens.split_token_span(eoi);

    let parser = program(text, file_id);
    parser.parse(input)
}

type Ctx<'a> = Full<Rich<'a, TokenKind>, (), ()>;

fn list<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>, T: Clone>(
    elem: impl Parser<'a, I, T, Ctx<'a>> + Clone,
    file_id: FileId,
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
) -> impl Parser<'a, I, Vec<Struct>, Ctx<'a>> {
    strukt(input, file_id).repeated().collect()
}

fn strukt<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>>(
    input: &'a str,
    file_id: FileId,
) -> impl Parser<'a, I, Struct, Ctx<'a>> {
    recursive(move |strukt| {
        let typ = recursive(move |typ| {
            let named = upper_name(input)
                .then(
                    list(typ.clone(), file_id)
                        .delimited_by(just(TokenKind::LeftSquare), just(TokenKind::RightSquare))
                        .or_not()
                        .map(|list| list.unwrap_or_default()),
                )
                .map_with(move |(name, children), e| Type {
                    kind: TypeKind::Named(name),
                    span: get_span(e, file_id),
                    children,
                });
            let generic = name(input)
                .map_with(move |name, e| Type::base(TypeKind::Generic(name), get_span(e, file_id)));
            let slice = just(TokenKind::LeftSquare)
                .ignore_then(just(TokenKind::RightSquare))
                .ignore_then(typ.clone())
                .map_with(move |elem, e| Type::slice(elem, get_span(e, file_id)));
            generic.or(named).or(slice)
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
                let var = name(input)
                    .map_with(move |name, e| Expr::Var(name, None, get_span(e, file_id)));

                let func_or_constructor = upper_name(input)
                    .then(
                        list(typ.clone(), file_id)
                            .delimited_by(just(TokenKind::LeftSquare), just(TokenKind::RightSquare))
                            .or_not()
                            .map(|list| list.unwrap_or_default()),
                    )
                    .then(just(TokenKind::Dot).ignore_then(name(input)).or_not())
                    .map_with(move |((struct_name, generics), func_name), e| {
                        if let Some(func_name) = func_name {
                            Expr::Func(struct_name, func_name, None, get_span(e, file_id))
                        } else {
                            let constructor = constructor_name(&struct_name);
                            Expr::Func(struct_name, constructor, None, get_span(e, file_id))
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
                let base = literal
                    .or(var)
                    .or(func_or_constructor)
                    .or(block)
                    .or(_if)
                    .or(_while)
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
                let deref_op = just(TokenKind::Star)
                    .and_is(just(TokenKind::Star).then(atom_start.clone().not()));
                let ref_op = just(TokenKind::Ampersand)
                    .and_is(just(TokenKind::Ampersand).then(atom_start.clone().not()));
                let bit_and_op = just(TokenKind::Ampersand)
                    .and_is(just(TokenKind::Ampersand).then(atom_start.clone()));
                base.pratt((
                    postfix(
                        100,
                        just(TokenKind::Dot)
                            .ignore_then(name(input))
                            .then(call_args.clone().or_not()),
                        move |lhs, (name, args), e| {
                            if let Some(args) = args {
                                Expr::MethodCall(
                                    Box::new(lhs),
                                    name,
                                    args,
                                    None,
                                    get_span(e, file_id),
                                )
                            } else {
                                Expr::Field(Box::new(lhs), name, None, get_span(e, file_id))
                            }
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
        just(TokenKind::Struct)
            .ignore_then(upper_name(input))
            .then(generics)
            .then_ignore(just(TokenKind::LeftBrace))
            .then(field.repeated().collect::<Vec<_>>())
            .then(func.repeated().collect::<Vec<_>>())
            .then_ignore(just(TokenKind::RightBrace))
            .map_with(move |(((name, generics), fields), funcs), e| Struct {
                name,
                generics: generics,
                fields: fields.into_iter().collect(),
                funcs: funcs
                    .into_iter()
                    .map(|func| (FuncSpec::named(func.name.clone()), func))
                    .collect(),
                span: get_span(e, file_id),
            })
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
        let source = r#"
            struct Main {
                func main(): Int = 3
            }
        "#;
        test_parse(source);
    }

    #[test]
    fn feed_cats() {
        let source = r#"
            struct Cat[t] {
                inner: t
            }

            struct Main[t] {
                func main(c: Cat[t]): Unit = Cat[t].feed(c, 3)
            }
        "#;
        test_parse(source);
    }

    #[test]
    fn cors() {
        let source = r#"
            struct Main {
                cor foo(y: Int): Int = {
                    yield;
                    let x = y;
                    yield
                    x
                }

                cor bar(x: Int): Int = {
                    yield;
                    let q = Main.foo(Int.add(x, 1))!;
                    q
                }

                func poll_twice(sm: Ptr[Main_bar], result: Ptr[Int]): Unit = {
                    Main_bar.poll(sm, result);
                    Main_bar.poll(sm, result);
                }

                func main(): Int = {
                    let sm = Main.bar(3);
                }
            }
        "#;
    }

    #[test]
    fn blocks_loops_ifs() {
        let source = r#"
            struct Main {
                func main(): Int = {
                    let total = 0;
                    let i = 0;
                    while Int.less_than(i, 10) {
                        if Int.less_than(i, 5) {
                            set total = Int.add(total, i);
                        } else {}
                        set i = Int.add(i, 1);
                    }
                    total
                }
            }
        "#;
    }

    #[test]
    fn getters_ref() {
        let source = r#"
            struct Pair[x, y] {
                a: x
                b: y
                func first(p: Pair[x, y]): x = p.a
            }

            struct Foo {
                func f(p: Pair[Int, Int]): Int = {
                    let x = p.a;
                    Ptr.store(&x, 7);
                    x
                }
            }
        "#;
    }

    #[test]
    fn ufcs() {
        let source = r#"
            struct Foo {
                func double(x: Int): Int = Int.add(x, x)
                func f(): Bool = {
                    3.less_than(1.double())
                }
            }
        "#;
    }
}
