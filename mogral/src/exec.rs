use inkwell::{
    execution_engine::{ExecutionEngine, JitFunction},
    OptimizationLevel,
};

use crate::MglModule;

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type Func = unsafe extern "C" fn(f64) -> f64;

fn jit_compile_main<'ctx>(
    execution_engine: &ExecutionEngine<'ctx>,
) -> Option<JitFunction<'ctx, Func>> {
    unsafe { execution_engine.get_function("main").ok() }
}

pub fn exec<'ctx>(module: &MglModule<'ctx>, arg: f64) -> Result<f64, Box<dyn std::error::Error>> {
    let execution_engine = module
        .0
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let sum = jit_compile_main(&execution_engine).ok_or("Unable to JIT compile `main`")?;
    unsafe { Ok(sum.call(arg)) }
}
