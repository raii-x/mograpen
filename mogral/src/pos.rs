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

/// エラーで返されるソースコード上の位置情報を行番号と列番号に変換するための構造体
pub struct SourcePosConverter {
    line_pos_vec: Vec<usize>,
}

impl SourcePosConverter {
    pub fn new(source: &str) -> Self {
        // ソースコードの各行に対応する位置をline_pos_vecに格納する
        let mut line_pos_vec = Vec::new();
        line_pos_vec.push(0);
        for (i, c) in source.bytes().enumerate() {
            if c == b'\n' {
                line_pos_vec.push(i + 1);
            }
        }

        Self { line_pos_vec }
    }

    /// 位置を行番号と列番号に変換する
    /// 行番号と列番号は1から始まる
    pub fn pos_to_line_col(&self, pos: usize) -> (usize, usize) {
        // line_pos_vecのindexに変換する
        let line_index = match self.line_pos_vec.binary_search(&pos) {
            Ok(i) => i,
            Err(i) => i - 1,
        };

        (line_index + 1, pos - self.line_pos_vec[line_index] + 1)
    }
}
