use annotate_snippets::{AnnotationKind, Group, Level, Renderer, Snippet};

use crate::ast::Span;

pub fn print_errors(text: &str, errors: Vec<(String, Span)>) {
    let mut s = Snippet::source(text);
    s = s.annotations(errors.iter().map(|(error, span)| {
        AnnotationKind::Primary
            .span(span.start..span.end)
            .label(error)
    }));
    let report = Level::ERROR.primary_title("Khaki Error").element(s);
    println!("{}", Renderer::styled().render(&[report]));
}
