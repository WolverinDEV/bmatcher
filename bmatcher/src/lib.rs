/*!
A flexible and efficient binary pattern matching library designed to help you search and match binary data.

# How to create patterns?
In order to create a pattern you need some knowledge with reverse engineering programs.

A guide on how to create signatures can be found here: <https://wiki.alliedmods.net/Signature_scanning#Finding_the_Signature_of_a_Function>

*/
#![doc = include_str!("../GRAMMA.MD")]
#![cfg_attr(not(test), no_std)]
pub use bmatcher_core::*;
pub use bmatcher_proc::pattern;
