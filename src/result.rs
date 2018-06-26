/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::{self, ptr};
use std::os::raw::{
    c_char,
};


use string::{
    string_to_c_char,
};


/// Represents a C-compatible error code.
///
/// Two values must be defined: one that represents success, and one that
/// represents an internal panic in the library (note that panics may not unwind
/// across the FFI boundary, and so we catch them for you when you use the
/// functions in this file).
pub trait CErrorCode: Copy + std::fmt::Debug {
    /// Should return the error code value representing success
    fn success_code() -> Self;
    /// Should return the error code value representing failure
    fn panic_code() -> Self;
}

/// An error struct containing an error code and a description string. Callers
/// should create values of this type locally and pass pointers to them in as
/// the last argument of functions which may fail.
///
/// You are expected to implement `From<MyError> for ExternError<MyErrorCode>`
/// for your error type. Expected usage might look something like this:
///
/// ```rust,no_run
/// #[repr(C)]
/// enum MyErrorCode {
///     NoError = 0,
///     InternalPanic = 1,
///     SomeErrorA = 2,
///     OtherErrorB = 3,
///     // ...
/// }
///
/// impl CErrorCode for MyErrorCode {
///     fn success_code() -> Self { MyErrorCode::NoError }
///     fn panic_code() -> Self { MyErrorCode::InternalPanic }
/// }
///
/// fn get_code(e: &MyError) -> MyErrorCode {
///     // ...
/// }
///
/// impl From<MyError> for ExternError<MyErrorCode> {
///     fn from(e: MyError) -> Self {
///         Self {
///             code: get_code(&e),
///             message: string_to_c_char(format!("{}", e)),
///         }
///     }
/// }
/// ```
///
/// Then, if you had a function `Foobar::frobincate(&self, data: &str) -> Result<Frobnication, MyError>`,
/// which you would like to expose over the FFI, you might do so as follows:
///
/// ```rust,no_run
/// #[no_mangle]
/// pub unsafe extern "C" fn frobnicate_foobar(
///     foobar: *const Foobar,
///     frobnication_data: *const c_char,
///     out_error: *mut ExternError<MyErrorCode>
/// ) -> *mut Frobnication {
///     call_with_result(error, || {
///         assert_pointer_not_null!(foobar);
///         let foobar = &*foobar;
///         foobar.frobnicate()
///     })
/// }
/// ```
///
/// # Safety
///
/// In the case that an error occurs, callers are responsible for freeing the
/// string stored in `message`. A destructor, `destroy_c_char`, is provided for
/// this purpose.
#[repr(C)]
#[derive(Debug)]
pub struct ExternError<EC> where EC: CErrorCode {
    pub code: EC,
    pub message: *mut c_char,
}

impl<EC: CErrorCode> Default for ExternError<EC> {
    fn default() -> Self {
        ExternError {
            code: CErrorCode::success_code(),
            message: ptr::null_mut(),
        }
    }
}

// This is the `Err` of std::thread::Result, which is what
// `std::panic::catch_unwind` returns.
impl<EC: CErrorCode> From<Box<std::any::Any + Send + 'static>> for ExternError<EC> {
    fn from(e: Box<std::any::Any + Send + 'static>) -> ExternError<EC> {
        // The documentation suggests that it will usually be a str or String.
        let message = if let Some(s) = e.downcast_ref::<&'static str>() {
            string_to_c_char(*s)
        } else if let Some(s) = e.downcast_ref::<String>() {
            string_to_c_char(s.clone())
        } else {
            // Note that it's important that this be allocated on the heap,
            // since we'll free it later!
            string_to_c_char("Unknown panic!")
        };

        ExternError {
            code: CErrorCode::panic_code(),
            message,
        }
    }
}

/// Call a function returning Result<T, E> inside catch_unwind, writing any error
/// or panic into ExternError.
///
/// In the case the call returns an error, information about this will be
/// written into the ExternError, and a null pointer will be returned.
///
/// In the case that the call succeeds, then the ExternError will have
/// `code == CErrorCode::success_code()` and `message == ptr::null_mut()`.
///
/// Note that we allow out_error to be null (it's not like we can panic if it's
/// not...), but *highly* discourage doing so. We will log error information to
/// stderr in the case that something goes wrong and you fail to provide an
/// error output.
///
/// Note: it's undefined behavior (e.g. very bad) to panic across the FFI
/// boundary, so it's important that we wrap calls that may fail in catch_unwind
/// like this.
pub unsafe fn call_with_result<R, F, E, EC>(out_error: *mut ExternError<EC>, callback: F) -> *mut R
where
    F: std::panic::UnwindSafe + FnOnce() -> Result<R, E>,
    E: Into<ExternError<EC>>,
    EC: CErrorCode,
{
    try_call_with_result(out_error, callback)
        .map(|v| Box::into_raw(Box::new(v)))
        .unwrap_or(ptr::null_mut())
}

/// A version of call_with_result for the cases when `R` is a type you'd like
/// to return directly to C. For example, a `*mut c_char`, or a `#[repr(C)]`
/// struct.
///
/// This requires you provide a default value to return in the error case.
pub unsafe fn call_with_result_by_value<R, F, E, EC>(out_error: *mut ExternError<EC>, default: R, callback: F) -> R
where
    F: std::panic::UnwindSafe + FnOnce() -> Result<R, E>,
    E: Into<ExternError<EC>>,
    EC: CErrorCode
{
    try_call_with_result(out_error, callback).unwrap_or(default)
}

/// Helper for the fairly common case where we want to return a string to C.
pub unsafe fn call_with_string_result<R, F, E, EC>(out_error: *mut ExternError<EC>, callback: F) -> *mut c_char
where
    F: std::panic::UnwindSafe + FnOnce() -> Result<R, E>,
    R: Into<String>,
    E: Into<ExternError<EC>>,
    EC: CErrorCode,
{
    call_with_result_by_value(out_error, ptr::null_mut(), || {
        callback().map(string_to_c_char)
    })
}

/// Common code between the various `call_with_result*`s.
pub unsafe fn try_call_with_result<R, F, E, EC>(out_error: *mut ExternError<EC>, callback: F) -> Option<R>
where
    F: std::panic::UnwindSafe + FnOnce() -> Result<R, E>,
    E: Into<ExternError<EC>>,
    EC: CErrorCode,
{
    let res: std::thread::Result<(ExternError<EC>, Option<R>)> =
        std::panic::catch_unwind(|| match callback() {
            Ok(v) => (Default::default(), Some(v)),
            Err(e) => (e.into(), None),
        });
    match res {
        Ok((err, o)) => {
            if !out_error.is_null() {
                let eref = &mut *out_error;
                *eref = err;
            } else {
                eprintln!(
                    "Warning: an error occurred but no error parameter was given: {:?}",
                    err
                );
            }
            o
        }
        Err(e) => {
            if !out_error.is_null() {
                let eref = &mut *out_error;
                *eref = e.into();
            } else {
                let err: ExternError<EC> = e.into();
                eprintln!(
                    "Warning: a panic occurred but no error parameter was given: {:?}",
                    err
                );
            }
            None
        }
    }
}
