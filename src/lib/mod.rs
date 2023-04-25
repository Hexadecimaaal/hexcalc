#![no_std]
#![feature(box_syntax, box_patterns)]
#![feature(wrapping_int_impl)]
extern crate alloc;

use alloc_cortex_m::CortexMHeap;
#[cfg_attr(not(test), global_allocator)]
#[allow(dead_code)]
static ALLOCATOR : CortexMHeap = CortexMHeap::empty();

pub mod errors;
pub mod expr;
pub mod intrinsics;
pub mod state;
pub mod typing;

pub mod parsing;

pub mod tests;

// use alloc::collections::VecDeque;

// type Stack<'a> = VecDeque<Expr<'a>>;

// fn eval<'a>(s : &mut Stack<'a>, v : Expr) {
//   match v {
//     Expr::Lit(x) => s.push_front(Expr::Lit(x)),
//     Expr::Func(f) => f(s),
//     Expr::App(func, val) =>
//   }
// }

// fn plus(s : &mut Stack) -> Result<(), Error> {
//   let (a, b) = (s.pop().ok_or(Err(Error::EmptyStack)),
//                 s.pop().ok_or(Err(Error::EmptyStack)));
//   match (a, b) {

//   }
// }
