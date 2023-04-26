use inkwell::{context::Context, module::Module};

// 外部からinkwellを隠蔽するため、ContextとModuleをラップする

#[derive(Debug, PartialEq, Eq)]
pub struct MglContext(pub(crate) Context);

impl MglContext {
    pub fn new() -> Self {
        Self(Context::create())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MglModule<'ctx>(pub(crate) Module<'ctx>);

impl<'ctx> MglModule<'ctx> {
    pub fn to_string(&self) -> String {
        self.0.to_string()
    }
}
