use crate::expr::Expr;
use alloc::{ sync::Arc, string::String };

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
  Type,
  NonRedux(Arc<Expr>),
  EmptyStack(usize),
  Undefined(String),
  Exists(String)
}
