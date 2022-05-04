use crate::expr::Expr;
use alloc::{ string::String };

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
  Type,
  NonRedux(Expr),
  EmptyStack(usize),
  Undefined(String),
  Exists(String)
}
