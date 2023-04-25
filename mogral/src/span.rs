#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub l: usize,
    pub r: usize,
}

#[derive(Debug)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            item: &self.item,
            span: self.span,
        }
    }
}
