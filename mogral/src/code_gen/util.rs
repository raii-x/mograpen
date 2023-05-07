/// OptionがSomeならば中身を取り出し、
/// NoneならばOk(None)をreturnする
#[macro_export]
macro_rules! unwrap_or_return {
    ( $e:expr ) => {
        match $e {
            Some(x) => x,
            None => return Ok(None),
        }
    };
}
