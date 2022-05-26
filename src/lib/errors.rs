use crate::expr::Expr;
use crate::typing::{Type};
use alloc::boxed::Box;
use alloc::string::String;
use alloc::vec::Vec;

pub trait Err: alloc::fmt::Debug {}

pub trait CheckingError: alloc::fmt::Debug {}

impl<'a, T> From<Box<T>> for Box<dyn CheckingError + 'a>
where
  T : CheckingError + 'a,
{
  fn from(f : Box<T>) -> Self { f }
}

impl<T> Err for T where T : CheckingError {}

#[derive(Clone, Debug, PartialEq)]
pub struct Incomplete(pub Vec<String>);
impl CheckingError for Incomplete {}

#[derive(Clone, Debug, PartialEq)]
pub struct Cyclic(pub Type, pub Type);
impl CheckingError for Cyclic {}

#[derive(Clone, Debug, PartialEq)]
pub struct Unbound(pub Type);
impl CheckingError for Unbound {}

#[derive(Clone, Debug, PartialEq)]
pub struct Subtype(pub Type, pub Type);
impl CheckingError for Subtype {}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
  Type,
  NonRedux(Expr),
  EmptyStack(usize),
  Undefined(String),
  Exists(String),
  Range(usize, usize),
}

impl Err for Error {}
