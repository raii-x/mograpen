use strum_macros::IntoStaticStr;

/// MograLで使用する型
#[derive(Debug, PartialEq, Eq, Clone, Copy, IntoStaticStr)]
pub enum MglType {
    #[strum(serialize = "unit")]
    Unit,
    #[strum(serialize = "double")]
    Double,
    #[strum(serialize = "bool")]
    Bool,
}
