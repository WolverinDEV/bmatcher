/*!
A flexible and efficient binary pattern matching library designed to help you search and match binary data.

# BMatchers synatx for patterns
An exhausive overview of the pattern syntax and operads can be found here: [doc_pattern_syntax].

# How to create patterns?
In order to create a pattern you need some knowledge with reverse engineering programs.

A guide on how to create signatures can be found here: <https://wiki.alliedmods.net/Signature_scanning#Finding_the_Signature_of_a_Function>
*/

#![cfg_attr(not(test), no_std)]
pub use bmatcher_core::*;
pub use bmatcher_proc::pattern;

#[doc = include_str!("../../GRAMMA.MD")]
pub fn doc_pattern_syntax() {}
