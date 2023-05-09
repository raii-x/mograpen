/// MaybeNeverがValueならば中身を取り出し、
/// NeverならばOk(Never)をreturnする
#[macro_export]
macro_rules! unwrap_never {
    ( $e:expr ) => {
        match $e {
            MaybeNever::Value(x) => x,
            MaybeNever::Never => return Ok(MaybeNever::Never),
        }
    };
}
