use std::ops::Range;

use chumsky::{
    IterParser, ParseResult, Parser,
    error::Rich,
    extra::{Full, ParserExtra},
    input::{Input, MapExtra},
    prelude::{just, recursive},
    span::SimpleSpan,
};

use logos::Logos;

use crate::{
    ast::{Expr, Func, Literal, Op, Span, Stmt, Struct, Type, TypeKind, constructor_name},
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
) -> ParseResult<Vec<Struct>, Rich<'a, TokenKind>> {
    let eoi = SimpleSpan::from(text.len()..text.len());
    let input = tokens.split_token_span(eoi);

    let parser = program(text);
    parser.parse(input)
}

type Ctx<'a> = Full<Rich<'a, TokenKind>, (), ()>;

fn list<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>, T: Clone>(
    elem: impl Parser<'a, I, T, Ctx<'a>> + Clone,
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
) -> Span {
    let span: SimpleSpan = extra.span();
    span.into()
}

fn program<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>>(
    input: &'a str,
) -> impl Parser<'a, I, Vec<Struct>, Ctx<'a>> {
    strukt(input).repeated().collect()
}

fn strukt<'a, I: Input<'a, Token = TokenKind, Span = SimpleSpan>>(
    input: &'a str,
) -> impl Parser<'a, I, Struct, Ctx<'a>> {
    let typ = recursive(|typ| {
        let named = upper_name(input)
            .then(
                list(typ.clone())
                    .delimited_by(just(TokenKind::LeftSquare), just(TokenKind::RightSquare))
                    .or_not()
                    .map(|list| list.unwrap_or_default()),
            )
            .map_with(|(name, children), e| Type {
                kind: TypeKind::Named(name),
                span: get_span(e),
                children,
            });
        let generic =
            name(input).map_with(|name, e| Type::base(TypeKind::Generic(name), get_span(e)));
        generic.or(named)
    });

    let expr = recursive(|expr| {
        let number = num(input).map_with(|num, e| Literal::Number(num, get_span(e)));
        let _true = just(TokenKind::True).map_with(|_, e| Literal::Bool(true, get_span(e)));
        let _false = just(TokenKind::False).map_with(|_, e| Literal::Bool(false, get_span(e)));
        let literal = number
            .or(_true)
            .or(_false)
            .map(|lit| Expr::Literal(lit, None));
        let var = name(input).map_with(|name, e| Expr::Var(name, None, get_span(e)));

        let func_or_constructor = upper_name(input)
            .then(
                list(typ.clone())
                    .delimited_by(just(TokenKind::LeftSquare), just(TokenKind::RightSquare))
                    .or_not()
                    .map(|list| list.unwrap_or_default()),
            )
            .then(just(TokenKind::Dot).ignore_then(name(input)).or_not())
            .map_with(|((struct_name, generics), func_name), e| {
                if let Some(func_name) = func_name {
                    Expr::Func(struct_name, func_name, None, get_span(e))
                } else {
                    let constructor = constructor_name(&struct_name);
                    Expr::Func(struct_name, constructor, None, get_span(e))
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
            .map_with(|(mut stmts, last_stmt), e| {
                let span = get_span(e);
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
        let _ref = just(TokenKind::Ampersand)
            .then(expr.clone())
            .map_with(|(_, expr), e| Expr::Op(Op::Ref, vec![expr], None, get_span(e)));
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
            .map_with(|(cond, (if_true, if_false)), e| {
                Expr::Op(Op::If, vec![cond, if_true, if_false], None, get_span(e))
            });
        let _while = just(TokenKind::While)
            .ignore_then(expr.clone())
            .then(block.clone())
            .map_with(|(cond, body), e| Expr::Op(Op::While, vec![cond, body], None, get_span(e)));
        let _yield =
            just(TokenKind::Yield).map_with(|_, e| Expr::Op(Op::Yield, vec![], None, get_span(e)));
        let base = literal
            .or(var)
            .or(func_or_constructor)
            .or(block)
            .or(_yield)
            .or(_ref)
            .or(_if)
            .or(_while);

        enum Modif {
            Call(Vec<Expr>, Span),
            Await(Span),
            Deref(Span),
            Field(String, Span),
        }

        let call_modif = list(expr.clone())
            .delimited_by(just(TokenKind::LeftParen), just(TokenKind::RightParen))
            .map_with(|args, e| Modif::Call(args, get_span(e)));
        let await_modif = just(TokenKind::Bang).map_with(|_, e| Modif::Await(get_span(e)));
        let deref_modif = just(TokenKind::Star).map_with(|_, e| Modif::Deref(get_span(e)));
        let field_modif = just(TokenKind::Dot)
            .ignore_then(name(input))
            .map_with(|name, e| Modif::Field(name, get_span(e)));
        let modif = call_modif.or(await_modif).or(deref_modif).or(field_modif);

        base.then(modif.repeated().collect::<Vec<_>>())
            .map_with(|(mut expr, modifs), e| {
                for modif in modifs {
                    expr = match modif {
                        Modif::Call(args, span) => Expr::Call(Box::new(expr), args, None, span),
                        Modif::Await(span) => Expr::Op(Op::Await, vec![expr], None, span),
                        Modif::Deref(span) => Expr::Op(Op::Deref, vec![expr], None, span),
                        Modif::Field(field_name, span) => {
                            Expr::Field(Box::new(expr), field_name, None, span)
                        }
                    };
                }
                expr
            })
    });

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
        .then(list(arg).delimited_by(just(TokenKind::LeftParen), just(TokenKind::RightParen)))
        .then_ignore(just(TokenKind::Colon))
        .then(typ.clone())
        .then_ignore(just(TokenKind::Equals))
        .then(expr)
        .map(|((((is_cor, name), args), result), body)| Func {
            name,
            args,
            result,
            body,
            is_cor,
        });
    just(TokenKind::Struct)
        .ignore_then(upper_name(input))
        .then(
            list(name(input))
                .delimited_by(just(TokenKind::LeftSquare), just(TokenKind::RightSquare))
                .or_not(),
        )
        .then_ignore(just(TokenKind::LeftBrace))
        .then(field.repeated().collect::<Vec<_>>())
        .then(func.repeated().collect::<Vec<_>>())
        .then_ignore(just(TokenKind::RightBrace))
        .map_with(|(((name, generics), fields), funcs), e| Struct {
            name,
            generics: generics.unwrap_or_default(),
            fields: fields.into_iter().collect(),
            funcs: funcs
                .into_iter()
                .map(|func| (func.name.clone(), func))
                .collect(),
            span: get_span(e),
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
        let _ = parse_program(text, &tokens).unwrap();
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
