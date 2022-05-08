use crate::expr::Expr;
use crate::typing::{Condition, Type};
use alloc::string::String;
use alloc::vec::Vec;

pub trait Err {}

pub trait CheckingError {}

impl<T> Err for T where T : CheckingError {}

#[derive(Clone, Debug, PartialEq)]
pub struct Incomplete(pub Vec<String>);
impl CheckingError for Incomplete {}

#[derive(Clone, Debug, PartialEq)]
pub struct Cyclic(pub Type, pub Type);
impl CheckingError for Cyclic {}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
  Type,
  NonRedux(Expr),
  EmptyStack(usize),
  Undefined(String),
  Exists(String),
  Range(usize, usize),
}
