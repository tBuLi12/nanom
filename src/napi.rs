use std::{
    any, error,
    ffi::c_void,
    fmt::Debug,
    hint,
    panic::{self, AssertUnwindSafe},
    ptr, result,
};

use napi_sys::{napi_deferred, napi_env, napi_status, napi_value, ValueType};

pub type Result<T> = result::Result<T, Status>;
pub type Value = napi_value;
pub type Deferred = napi_deferred;

pub struct Status {
    value: napi_status,
}

impl Debug for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use napi_sys::Status::*;

        #[allow(non_upper_case_globals)]
        let name = match self.value {
            napi_ok => "napi_ok",
            napi_invalid_arg => "napi_invalid_arg",
            napi_object_expected => "napi_object_expected",
            napi_string_expected => "napi_string_expected",
            napi_name_expected => "napi_name_expected",
            napi_function_expected => "napi_function_expected",
            napi_number_expected => "napi_number_expected",
            napi_boolean_expected => "napi_boolean_expected",
            napi_array_expected => "napi_array_expected",
            napi_generic_failure => "napi_generic_failure",
            napi_pending_exception => "napi_pending_exception",
            napi_cancelled => "napi_cancelled",
            napi_escape_called_twice => "napi_escape_called_twice",
            napi_handle_scope_mismatch => "napi_handle_scope_mismatch",
            napi_callback_scope_mismatch => "napi_callback_scope_mismatch",
            napi_queue_full => "napi_queue_full",
            napi_closing => "napi_closing",
            napi_bigint_expected => "napi_bigint_expected",
            napi_date_expected => "napi_date_expected",
            napi_arraybuffer_expected => "napi_arraybuffer_expected",
            napi_detachable_arraybuffer_expected => "napi_detachable_arraybuffer_expected",
            napi_would_deadlock => "napi_would_deadlock",
            napi_no_external_buffers_allowed => "napi_no_external_buffers_allowed",
            _ => "unknown napi status",
        };

        write!(f, "{name}")
    }
}

fn as_result(status: napi_sys::napi_status) -> Result<()> {
    if status == napi_sys::Status::napi_ok {
        Ok(())
    } else {
        Err(Status { value: status })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Null,
    Boolean,
    Number,
    BigInt,
    String,
    Symbol,
    Object,
    Function,
    External,
    Undefined,
}

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct Env {
    inner: napi_env,
}

impl Env {
    pub unsafe fn set_instance_data<T: 'static>(&self, data: T) -> Result<()> {
        let ptr = Box::into_raw(Box::new(data));

        let result = as_result(napi_sys::napi_set_instance_data(
            self.inner,
            ptr as *mut c_void,
            Some(drop_data::<T>),
            ptr::null_mut(),
        ));

        if result.is_err() {
            drop(Box::from_raw(ptr));
        }

        result
    }

    pub unsafe fn get_instance_data<T>(&self) -> Result<*mut T> {
        let mut data = ptr::null_mut();

        as_result(napi_sys::napi_get_instance_data(self.inner, &mut data))?;

        Ok(data as *mut T)
    }

    pub unsafe fn throw(&self, error: Value) -> Result<()> {
        as_result(napi_sys::napi_throw(self.inner, error))
    }

    pub unsafe fn must_throw(&self, message: &str) -> Value {
        let Ok(js_message) = self.create_string(message) else {
            fatal_error(
                "",
                &format!(
                    "napi create string failed when attempting to throw exception: {}",
                    message
                ),
            );
        };

        if self.throw(js_message).is_err() {
            fatal_error(
                "",
                &format!(
                    "napi throw failed when attempting to throw exception: {}",
                    message
                ),
            );
        }

        ptr::null_mut()
    }

    pub unsafe fn get_pending_exception(&self) -> Option<Value> {
        let mut is_pending = false;

        let result = as_result(napi_sys::napi_is_exception_pending(
            self.inner,
            &mut is_pending,
        ));

        if let Err(err) = result {
            fatal_error("", &format!("failed to check pending exception: {err:?}"));
        }

        if is_pending {
            Some(ptr::null_mut())
        } else {
            None
        }
    }

    pub unsafe fn create_object(&self) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_create_object(self.inner, &mut result))?;

        Ok(result)
    }

    pub unsafe fn get_property(&self, value: Value, prop: Value) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_get_property(
            self.inner,
            value,
            prop,
            &mut result,
        ))?;

        Ok(result)
    }

    pub unsafe fn get_property_names(&self, value: Value) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_get_property_names(
            self.inner,
            value,
            &mut result,
        ))?;

        Ok(result)
    }

    pub unsafe fn get_array_len(&self, value: Value) -> Result<u32> {
        let mut len = 0;

        as_result(napi_sys::napi_get_array_length(self.inner, value, &mut len))?;

        Ok(len)
    }

    pub unsafe fn get_element(&self, array: Value, idx: u32) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_get_element(
            self.inner,
            array,
            idx,
            &mut result,
        ))?;

        Ok(result)
    }

    pub unsafe fn set_element(&self, array: Value, idx: u32, element: Value) -> Result<()> {
        as_result(napi_sys::napi_set_element(self.inner, array, idx, element))
    }

    pub unsafe fn get_array_iter(
        &self,
        value: Value,
    ) -> Result<impl Iterator<Item = Result<Value>>> {
        let len = self.get_array_len(value)?;

        let env = *self;

        Ok((0..len).map(move |i| env.get_element(value, i)))
    }

    pub unsafe fn create_array(&self) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_create_array(self.inner, &mut result))?;

        Ok(result)
    }

    pub unsafe fn create_array_with_length(&self, length: usize) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_create_array_with_length(
            self.inner,
            length,
            &mut result,
        ))?;

        Ok(result)
    }

    pub unsafe fn create_array_buffer(&self, length: usize) -> Result<ArrayBuffer> {
        let mut ptr = ptr::null_mut();
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_create_arraybuffer(
            self.inner,
            length,
            &mut ptr,
            &mut result,
        ))?;

        Ok(ArrayBuffer {
            ptr: ptr::slice_from_raw_parts_mut(ptr as *mut u8, length),
            js: result,
        })
    }

    pub unsafe fn get_data_view(&self, data_view: Value) -> Result<*mut [u8]> {
        let mut len = 0;
        let mut data = ptr::null_mut();
        let mut array_buffer = ptr::null_mut();
        let mut offset = 0;

        as_result(napi_sys::napi_get_dataview_info(
            self.inner,
            data_view,
            &mut len,
            &mut data,
            &mut array_buffer,
            &mut offset,
        ))?;

        Ok(ptr::slice_from_raw_parts_mut(
            (data as *mut u8).add(offset),
            len,
        ))
    }

    pub unsafe fn create_external<T: 'static>(&self, data: T) -> Result<Value> {
        let mut external = ptr::null_mut();

        let ptr = Box::into_raw(Box::new(data));

        let result = as_result(napi_sys::napi_create_external(
            self.inner,
            ptr as *mut c_void,
            Some(drop_data::<T>),
            ptr::null_mut(),
            &mut external,
        ));

        if result.is_err() {
            drop(Box::from_raw(ptr));
        }

        result?;

        Ok(external)
    }

    pub unsafe fn set_property(&self, object: Value, key: Value, value: Value) -> Result<()> {
        as_result(napi_sys::napi_set_property(self.inner, object, key, value))
    }

    pub unsafe fn create_string(&self, value: &str) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_create_string_utf8(
            self.inner,
            value.as_ptr() as *const i8,
            value.len(),
            &mut result,
        ))?;

        Ok(result)
    }

    pub unsafe fn get_value_string(&self, value: Value) -> Result<String> {
        let mut len = 0;

        as_result(napi_sys::napi_get_value_string_utf8(
            self.inner,
            value,
            ptr::null_mut(),
            0,
            &mut len,
        ))?;

        let mut string = String::with_capacity(len + 1);

        as_result(napi_sys::napi_get_value_string_utf8(
            self.inner,
            value,
            string.as_mut_ptr() as *mut i8,
            string.capacity(),
            &mut len,
        ))?;

        string.as_mut_vec().set_len(len);

        Ok(string)
    }

    pub unsafe fn get_value_double(&self, value: Value) -> Result<f64> {
        let mut result = 0.0;

        as_result(napi_sys::napi_get_value_double(
            self.inner,
            value,
            &mut result,
        ))?;

        Ok(result)
    }

    pub unsafe fn create_double(&self, value: f64) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_create_double(self.inner, value, &mut result))?;

        Ok(result)
    }

    pub unsafe fn get_null(&self) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_get_null(self.inner, &mut result))?;

        Ok(result)
    }

    pub unsafe fn get_undefined(&self) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_get_undefined(self.inner, &mut result))?;

        Ok(result)
    }

    pub unsafe fn type_of(&self, value: Value) -> Result<Type> {
        let mut result = napi_sys::ValueType::napi_undefined;

        as_result(napi_sys::napi_typeof(self.inner, value, &mut result))?;

        Ok(match result {
            ValueType::napi_bigint => Type::BigInt,
            ValueType::napi_boolean => Type::Boolean,
            ValueType::napi_external => Type::External,
            ValueType::napi_function => Type::Function,
            ValueType::napi_null => Type::Null,
            ValueType::napi_number => Type::Number,
            ValueType::napi_object => Type::Object,
            ValueType::napi_string => Type::String,
            ValueType::napi_symbol => Type::Symbol,
            ValueType::napi_undefined => Type::Undefined,
            _ => hint::unreachable_unchecked(),
        })
    }

    pub unsafe fn get_boolean_value(&self, value: Value) -> Result<bool> {
        let mut result = false;

        as_result(napi_sys::napi_get_value_bool(
            self.inner,
            value,
            &mut result,
        ))?;

        Ok(result)
    }

    pub unsafe fn get_boolean(&self, value: bool) -> Result<Value> {
        let mut result = ptr::null_mut();

        as_result(napi_sys::napi_get_boolean(self.inner, value, &mut result))?;

        Ok(result)
    }

    pub unsafe fn create_function<
        const N: usize,
        E: error::Error,
        F: Fn(Env, [Value; N]) -> result::Result<Value, E> + 'static,
    >(
        &self,
        fun: F,
    ) -> Result<Value> {
        let name = any::type_name_of_val(&fun);

        let mut js_fun = ptr::null_mut();

        let ptr = Box::into_raw(Box::new(fun));

        let result = as_result(napi_sys::napi_create_function(
            self.inner,
            name.as_ptr() as *const i8,
            name.len(),
            Some(call_function::<N, E, F>),
            ptr as *mut c_void,
            &mut js_fun,
        ));

        if let Err(err) = result {
            drop(Box::from_raw(ptr));
            return Err(err);
        }

        // TODO: warn if this fails
        napi_sys::napi_add_finalizer(
            self.inner,
            js_fun,
            ptr as *mut c_void,
            Some(drop_data::<F>),
            ptr::null_mut(),
            ptr::null_mut(),
        );

        Ok(js_fun)
    }

    pub unsafe fn create_and_queue_async_work<W: AsyncWork>(&self, work: W) -> Result<()> {
        let data = Box::into_raw(Box::new((ptr::null_mut(), work)));

        let result = as_result(napi_sys::napi_create_async_work(
            self.inner,
            ptr::null_mut(),
            self.create_string(any::type_name::<W>())?,
            Some(exec_async_work::<W>),
            Some(complete_async_work::<W>),
            data as *mut c_void,
            &raw mut (*data).0,
        ));

        if let Err(err) = result {
            println!("Error creating async work");
            drop(Box::from_raw(data));
            return Err(err);
        }

        let result = as_result(napi_sys::napi_queue_async_work(self.inner, (*data).0));

        if let Err(err) = result {
            println!("Error queuing async work");
            drop(Box::from_raw(data));
            return Err(err);
        }

        Ok(())
    }

    pub unsafe fn create_promise(&self) -> Result<(Value, Deferred)> {
        let mut deferred = ptr::null_mut();
        let mut promise = ptr::null_mut();

        as_result(napi_sys::napi_create_promise(
            self.inner,
            &mut deferred,
            &mut promise,
        ))?;

        Ok((promise, deferred))
    }

    pub unsafe fn promise_resolve(&self, deferred: Deferred, value: Value) -> Result<()> {
        as_result(napi_sys::napi_resolve_deferred(self.inner, deferred, value))
    }

    pub unsafe fn promise_reject(&self, deferred: Deferred, value: Value) -> Result<()> {
        as_result(napi_sys::napi_reject_deferred(self.inner, deferred, value))
    }
}

unsafe extern "C" fn drop_data<T>(_: napi_env, data: *mut c_void, _: *mut c_void) {
    drop(Box::from_raw(data as *mut T));
}

unsafe extern "C" fn call_function<
    const N: usize,
    E: error::Error,
    F: Fn(Env, [Value; N]) -> result::Result<Value, E> + 'static,
>(
    env: napi_env,
    callback_info: napi_sys::napi_callback_info,
) -> Value {
    let mut args = [ptr::null_mut(); N];
    let mut argc = N;
    let mut this = ptr::null_mut();
    let mut data = ptr::null_mut();

    let get_info_result = as_result(napi_sys::napi_get_cb_info(
        env,
        callback_info,
        &mut argc,
        args.as_mut_ptr(),
        &mut this,
        &mut data,
    ));

    let env = Env { inner: env };

    if let Some(exception) = env.get_pending_exception() {
        return exception;
    }

    if let Err(get_info_result) = get_info_result {
        return env.must_throw(&format!(
            "napi get callback info failed: {get_info_result:?}"
        ));
    }

    if argc != N {
        return env.must_throw(&format!(
            "native function expected {N} arguments, but received {argc}"
        ));
    }

    let data = AssertUnwindSafe(data);

    match panic::catch_unwind(|| (*(*data as *mut F))(env, args)) {
        Ok(Ok(result)) => result,
        Ok(Err(error)) => env.must_throw(&format!("{error}")),
        Err(info) => {
            let message = if let Some(string) = info.downcast_ref::<String>() {
                &string
            } else if let Some(string) = info.downcast_ref::<&str>() {
                string
            } else {
                "Unknown panic"
            };

            env.must_throw(message)
        }
    }
}

pub trait AsyncWork: Send + 'static {
    fn exec(&mut self);
    fn complete(self, env: Env);
    fn failed(self, env: Env, status: Status);
}

unsafe extern "C" fn exec_async_work<W: AsyncWork>(_: napi_env, data: *mut c_void) {
    let work = data as *mut (napi_sys::napi_async_work, W);
    (*work).1.exec();
}

unsafe extern "C" fn complete_async_work<W: AsyncWork>(
    env: napi_env,
    status: napi_sys::napi_status,
    data: *mut c_void,
) {
    let env = Env { inner: env };
    let work = data as *mut (napi_sys::napi_async_work, W);
    let (work, data) = *Box::from_raw(work);

    match as_result(status) {
        Ok(()) => data.complete(env),
        Err(status) => data.failed(env, status),
    }

    napi_sys::napi_delete_async_work(env.inner, work);
}

pub struct ArrayBuffer {
    pub ptr: *mut [u8],
    pub js: Value,
}

pub unsafe fn fatal_error(location: &str, message: &str) -> ! {
    napi_sys::napi_fatal_error(
        location.as_ptr() as *const i8,
        location.len(),
        message.as_ptr() as *const i8,
        message.len(),
    );

    hint::unreachable_unchecked()
}
