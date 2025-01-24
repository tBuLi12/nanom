use std::{convert::Infallible, error, fmt, hint, mem};

pub mod napi;
pub mod typing;

pub use nanom_derive::JsObject;
use typing::Type;

#[macro_export]
macro_rules! register_module {
    ($module:expr) => {
        #[no_mangle]
        unsafe extern "C" fn napi_register_module_v1(
            env: $crate::napi::Env,
            exports: $crate::napi::Value,
        ) -> $crate::napi::Value {
            $crate::register_object_as_module(env, exports, $module)
        }
    };
}

pub unsafe fn register_object_as_module(
    env: napi::Env,
    exports: napi::Value,
    module: impl IntoJs,
) -> napi::Value {
    napi_sys::setup();

    let register = || -> Result<(), ConversionError> {
        let module = module.into_js(env)?;

        let names = env.get_property_names(module)?;
        for name in env.get_array_iter(names)? {
            let name = name?;
            let value = env.get_property(module, name)?;
            env.set_property(exports, name, value)?;
        }

        Ok(())
    };

    if let Err(err) = register() {
        return env.must_throw(&format!("module registration failed: {err}"));
    }

    exports
}

pub trait TsType {
    fn ts_type() -> Type;
}

pub trait FromJs: TsType + Sized {
    fn from_js(env: napi::Env, value: napi::Value) -> Result<Self, ConversionError>;
}

pub trait IntoJs: TsType {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError>;
}

pub trait JsObject: Sized {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError>;
    fn from_js(env: napi::Env, value: napi::Value) -> Result<Self, ConversionError>;
    fn ts_type() -> Type;
}

impl<T: JsObject> TsType for T {
    fn ts_type() -> Type {
        <T as JsObject>::ts_type()
    }
}

impl<T: JsObject> IntoJs for T {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError> {
        JsObject::into_js(self, env)
    }
}

impl<T: JsObject> FromJs for T {
    fn from_js(env: napi::Env, value: napi::Value) -> Result<Self, ConversionError> {
        JsObject::from_js(env, value)
    }
}

impl TsType for () {
    fn ts_type() -> Type {
        Type::Undefined
    }
}

impl FromJs for () {
    fn from_js(env: napi::Env, value: napi::Value) -> Result<Self, ConversionError> {
        unsafe {
            if env.type_of(value)? != napi::Type::Undefined {
                return Err(ConversionError::ExpectedUndefined);
            }
        }

        Ok(())
    }
}

impl IntoJs for () {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError> {
        unsafe { env.get_undefined() }.map_err(ConversionError::Napi)
    }
}

pub struct Null;

impl TsType for Null {
    fn ts_type() -> Type {
        Type::Null
    }
}

impl FromJs for Null {
    fn from_js(env: napi::Env, value: napi::Value) -> Result<Self, ConversionError> {
        unsafe {
            if env.type_of(value)? != napi::Type::Null {
                return Err(ConversionError::ExpectedNull);
            }
        }

        Ok(Null)
    }
}

impl IntoJs for Null {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError> {
        unsafe { env.get_null() }.map_err(ConversionError::Napi)
    }
}

impl FromJs for bool {
    fn from_js(env: napi::Env, value: napi::Value) -> Result<Self, ConversionError> {
        unsafe { env.get_boolean_value(value) }.map_err(ConversionError::Napi)
    }
}

impl IntoJs for bool {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError> {
        unsafe { env.get_boolean(self) }.map_err(ConversionError::Napi)
    }
}

impl TsType for bool {
    fn ts_type() -> Type {
        Type::Boolean
    }
}

impl<'a> TsType for &'a str {
    fn ts_type() -> Type {
        Type::String
    }
}

impl<'a> IntoJs for &'a str {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError> {
        unsafe { env.create_string(self) }.map_err(ConversionError::Napi)
    }
}

impl TsType for String {
    fn ts_type() -> Type {
        Type::String
    }
}

impl FromJs for String {
    fn from_js(env: napi::Env, value: napi::Value) -> Result<Self, ConversionError> {
        unsafe { env.get_value_string(value) }.map_err(ConversionError::Napi)
    }
}

impl IntoJs for String {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError> {
        IntoJs::into_js(&self[..], env)
    }
}

impl TsType for f64 {
    fn ts_type() -> Type {
        Type::Number
    }
}

impl FromJs for f64 {
    fn from_js(env: napi::Env, value: napi::Value) -> Result<Self, ConversionError> {
        unsafe { env.get_value_double(value) }.map_err(ConversionError::Napi)
    }
}

impl IntoJs for f64 {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError> {
        unsafe { env.create_double(self) }.map_err(ConversionError::Napi)
    }
}

impl<T: TsType> TsType for Vec<T> {
    fn ts_type() -> Type {
        Type::Array(Box::new(T::ts_type()))
    }
}

impl<T: IntoJs> IntoJs for Vec<T> {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError> {
        let array = unsafe { env.create_array_with_length(self.len()) }?;

        for (i, item) in self.into_iter().enumerate() {
            unsafe {
                env.set_element(array, i as u32, item.into_js(env)?)?;
            }
        }

        Ok(array)
    }
}

impl<T: FromJs> FromJs for Vec<T> {
    fn from_js(env: napi::Env, value: napi::Value) -> Result<Self, ConversionError> {
        let len = unsafe { env.get_array_len(value)? };

        let mut result = Vec::with_capacity(len as usize);

        for i in 0..len {
            result.push(T::from_js(env, unsafe { env.get_element(value, i)? })?);
        }

        Ok(result)
    }
}

impl TsType for *mut [u8] {
    fn ts_type() -> Type {
        Type::DataView
    }
}

impl FromJs for *mut [u8] {
    fn from_js(env: napi::Env, value: napi::Value) -> Result<Self, ConversionError> {
        unsafe { env.get_data_view(value) }.map_err(ConversionError::Napi)
    }
}

pub struct Function<F, A> {
    function: F,
    _args: std::marker::PhantomData<fn(A)>,
}

pub fn throwing_function<A, R, F: Fn(A) -> R>(function: F) -> Function<F, A> {
    Function {
        function,
        _args: std::marker::PhantomData,
    }
}

pub fn function<A, R, F: Fn(A) -> R + 'static>(
    function: F,
) -> Function<impl Fn(A) -> Result<R, Infallible> + 'static, A> {
    Function {
        function: move |arg| Ok(function(arg)),
        _args: std::marker::PhantomData,
    }
}

#[derive(Debug)]
pub enum ConversionError {
    Napi(napi::Status),
    InObjectField {
        field_name: &'static str,
        error: Box<ConversionError>,
    },
    InArrayElement {
        index: usize,
        error: Box<ConversionError>,
    },
    InKind(napi::Status),
    InEnumValue(Box<ConversionError>),
    ExpectedNull,
    ExpectedUndefined,
    InvalidKind(String),
}

impl From<napi::Status> for ConversionError {
    fn from(status: napi::Status) -> Self {
        ConversionError::Napi(status)
    }
}

impl error::Error for ConversionError {}
impl fmt::Display for ConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConversionError::Napi(status) => write!(f, "napi error: {status:?}"),
            ConversionError::InObjectField { field_name, error } => {
                write!(f, "in field \"{field_name}\": {error}")
            }
            ConversionError::InArrayElement { index, error } => {
                write!(f, "at array element {index}: {error}")
            }
            ConversionError::ExpectedNull => write!(f, "expected null"),
            ConversionError::ExpectedUndefined => write!(f, "expected undefined"),
            ConversionError::InvalidKind(kind) => write!(f, "invalid kind \"{kind}\""),
            ConversionError::InEnumValue(error) => {
                write!(f, "in enum value: {error}")
            }
            ConversionError::InKind(error) => write!(f, "in enum kind: {error:?}"),
        }
    }
}

#[derive(Debug)]
enum Error {
    Rust(Box<dyn error::Error>),
    InFunctionArgument {
        index: usize,
        error: ConversionError,
    },
    InFunctionReturnValue(ConversionError),
}

impl error::Error for Error {}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Rust(error) => write!(f, "rust error: {error}"),
            Error::InFunctionArgument { index, error } => {
                write!(f, "argument {index} conversion failed: {error}")
            }
            Error::InFunctionReturnValue(error) => {
                write!(f, "return value conversion failed: {error}")
            }
        }
    }
}

fn wrap_function<
    const N: usize,
    R: IntoJs,
    F: Fn(napi::Env, [napi::Value; N]) -> Result<R, Error> + 'static,
>(
    env: napi::Env,
    function: F,
) -> Result<napi::Value, ConversionError> {
    unsafe {
        env.create_function(move |env, args| {
            function(env, args)?
                .into_js(env)
                .map_err(Error::InFunctionReturnValue)
        })
        .map_err(ConversionError::Napi)
    }
}

macro_rules! impl_into_js_for_function {
    ($($args:ident $idx:literal),*) => {
        impl<$($args: TsType,)* R: TsType, E, F: Fn(($($args,)*)) -> Result<R, E>> TsType
            for Function<F, ($($args,)*)>
        {
            fn ts_type() -> Type {
                Type::Function {
                    args: vec![$($args::ts_type()),*],
                    return_type: Box::new(R::ts_type()),
                }
            }
        }

        impl<$($args: FromJs,)* R: IntoJs, E: error::Error + 'static, F: Fn(($($args,)*)) -> Result<R, E> + 'static> IntoJs
            for Function<F, ($($args,)*)>
        {
            fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError> {
                #[allow(non_snake_case, unused_variables)]
                wrap_function(env, move |env, [$($args),*]| {
                    (self.function)(($(FromJs::from_js(env, $args).map_err(|error| Error::InFunctionArgument { error, index: $idx })?,)*),).map_err(|err| Error::Rust(Box::new(err)))
                })
            }
        }
    };
}

impl_into_js_for_function!();
impl_into_js_for_function!(A1 1);
impl_into_js_for_function!(A1 1, A2 2);
impl_into_js_for_function!(A1 1, A2 2, A3 3);
impl_into_js_for_function!(A1 1, A2 2, A3 3, A4 4);
impl_into_js_for_function!(A1 1, A2 2, A3 3, A4 4, A5 5);
impl_into_js_for_function!(A1 1, A2 2, A3 3, A4 4, A5 5, A6 6);
impl_into_js_for_function!(A1 1, A2 2, A3 3, A4 4, A5 5, A6 6, A7 7);
impl_into_js_for_function!(A1 1, A2 2, A3 3, A4 4, A5 5, A6 6, A7 7, A8 8);
impl_into_js_for_function!(A1 1, A2 2, A3 3, A4 4, A5 5, A6 6, A7 7, A8 8, A9 9);
impl_into_js_for_function!(A1 1, A2 2, A3 3, A4 4, A5 5, A6 6, A7 7, A8 8, A9 9, A10 10);

#[macro_export]
macro_rules! object {
    ($($name:ident: $value:expr),*$(,)?) => {
        {
            #![allow(non_camel_case_types)]

            struct Object<$($name),*> {
                $($name: $name,)*
            }

            impl <$($name: ::nanom::IntoJs),*> ::nanom::IntoJs for Object<$($name),*> {
                fn into_js(self, env: ::nanom::napi::Env) -> ::std::result::Result<::nanom::napi::Value, ::nanom::ConversionError> {
                    unsafe {
                        let mut object = env.create_object()?;

                        $(
                            (|| -> ::std::result::Result<(), ::nanom::ConversionError> {
                                env.set_property(object, env.create_string(stringify!($name))?, self.$name.into_js(env)?)?;
                                ::std::result::Result::Ok(())
                            })().map_err(|err| ::nanom::ConversionError::InObjectField { error: Box::new(err), field_name: stringify!($name) })?;
                        )*

                        Ok(object)
                    }
                }
            }

            impl <$($name: ::nanom::TsType),*> ::nanom::TsType for Object<$($name),*> {
                fn ts_type() -> ::nanom::typing::Type {
                    let mut fields = ::std::collections::HashMap::new();

                    $(
                        fields.insert(stringify!($name).to_string(), <$name as ::nanom::TsType>::ts_type());
                    )*

                    ::nanom::typing::Type::Object(fields)
                }
            }

            Object {
                $($name: $value,)*
            }
        }
    };
}

pub struct AsyncWork<F> {
    fun: F,
}

impl<F> AsyncWork<F> {
    pub fn new(fun: F) -> Self {
        Self { fun }
    }
}

impl<F: FnOnce() -> R + Send + 'static, R: TsType + Send + 'static> TsType for AsyncWork<F> {
    fn ts_type() -> Type {
        Type::Promise(Box::new(R::ts_type()))
    }
}

impl<F: FnOnce() -> R + Send + 'static, R: IntoJs + Send + 'static> IntoJs for AsyncWork<F> {
    fn into_js(self, env: napi::Env) -> Result<napi::Value, ConversionError> {
        unsafe {
            let (promise, deferred) = env.create_promise()?;
            let work: AsyncWorkDeferred<F, R> = AsyncWorkDeferred {
                deferred,
                state: AsyncWorkState::Pending(self.fun),
            };

            println!("Creating async work");
            env.create_and_queue_async_work(work)?;

            Ok(promise)
        }
    }
}

enum AsyncWorkState<F, R> {
    Pending(F),
    Completed(R),
    None,
}

struct AsyncWorkDeferred<F, R> {
    state: AsyncWorkState<F, R>,
    deferred: napi::Deferred,
}

unsafe impl<F, R> Send for AsyncWorkDeferred<F, R> {}

impl<F, R> napi::AsyncWork for AsyncWorkDeferred<F, R>
where
    F: FnOnce() -> R + Send + 'static,
    R: IntoJs + Send + 'static,
{
    fn exec(&mut self) {
        let AsyncWorkState::Pending(fun) = mem::replace(&mut self.state, AsyncWorkState::None)
        else {
            unsafe { hint::unreachable_unchecked() };
        };
        let result = fun();
        self.state = AsyncWorkState::Completed(result);
    }

    fn complete(self, env: napi::Env) {
        unsafe {
            let AsyncWorkState::Completed(result) = self.state else {
                hint::unreachable_unchecked();
            };
            let result = match result.into_js(env) {
                Ok(result) => env.promise_resolve(self.deferred, result),
                Err(err) => {
                    let error_str = format!("{err}");
                    let Ok(error) = env.create_string(&error_str) else {
                        napi::fatal_error(
                            "",
                            &format!("failed to create error string when rejecting promise: {err}"),
                        );
                    };

                    env.promise_reject(self.deferred, error)
                }
            };

            if let Err(err) = result {
                napi::fatal_error("", &format!("failed to resolve promise: {err:?}"));
            }
        };
    }

    fn failed(self, env: napi::Env, status: napi::Status) {
        unsafe {
            let error_str = format!("{status:?}");
            let Ok(error) = env.create_string(&error_str) else {
                napi::fatal_error(
                    "",
                    &format!("failed to create error string when rejecting promise: {status:?}"),
                );
            };

            let result = env.promise_reject(self.deferred, error);
            if let Err(err) = result {
                napi::fatal_error("", &format!("failed to resolve promise: {err:?}"));
            }
        }
    }
}