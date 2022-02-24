#![cfg_attr(not(test), no_std)]
extern crate alloc;
extern crate lazy_static;

use alloc_cortex_m::CortexMHeap;
#[cfg_attr(not(test), global_allocator)]
#[allow(dead_code)]
static ALLOCATOR: CortexMHeap = CortexMHeap::empty();

pub mod expr;
pub mod state;
pub mod errors;

pub mod tests;

//use alloc::collections::VecDeque;


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

