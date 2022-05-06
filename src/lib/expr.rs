use crate::{errors::Error, state::Stmt};
use alloc::{boxed::Box, fmt::Display, string::String, vec, vec::Vec};
use core::{borrow::Borrow, num::Wrapping, panic};

#[derive(Clone, Copy)]
pub union Word64 {
  pub i : Wrapping<i64>,
  pub u : Wrapping<u64>,
  pub f : f64,
}

impl core::fmt::Debug for Word64 {
  fn fmt(&self, f : &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
    unsafe { core::fmt::Debug::fmt(&self.u, f) }
  }
}

impl PartialEq for Word64 {
  fn eq(&self, rhs : &Self) -> bool { unsafe { self.u == rhs.u } }
}

// #[derive(Clone)]
// pub enum Primitive {
//   V(PValue),
//   Fn11(&'static fn(PValue) -> PValue),
//   Fn21(&'static fn(PValue, PValue) -> PValue),
//   Fn12(&'static fn(PValue) -> (PValue, PValue)),
//   Fn22(&'static fn(PValue, PValue) -> (PValue, PValue)),
//   Fn31(&'static fn(PValue, PValue, PValue) -> PValue),
// }

// impl From<PValue> for Primitive {
//   fn from(v : PValue) -> Self { Primitive::V(v) }
// }

// impl core::fmt::Debug for Primitive {
//   fn fmt(&self, f : &mut core::fmt::Formatter<'_>) -> Result<(),
// core::fmt::Error> {     match self {
//       Primitive::V(v) => write!(f, "{:?}", v)?,
//       _ => write!(
//         f,
//         "#(function at {:?})",
//         self.borrow() as *const Primitive as usize
//       )?,
//     }
//     Ok(())
//   }
// }

// impl core::cmp::PartialEq for Primitive {
//   fn eq(&self, rhs : &Primitive) -> bool {
//     match (self, rhs) {
//       (Primitive::V(v1), Primitive::V(v2)) => v1 == v2,
//       _ => self as *const _ == rhs as *const _,
//     }
//   }
// }

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
  //  P(Primitive),
  Word(Word64),
  Text(String),

  Lam(String, Box<Expr>),
  Var(String),
  App(Box<Expr>, Box<Expr>),

  Cons(Box<Expr>, Box<Expr>),
  Fst(Box<Expr>),
  Snd(Box<Expr>),

  Match(Box<Expr>, Box<Expr>, Box<Expr>),
  Inl(Box<Expr>),
  Inr(Box<Expr>),

  Program(Vec<Stmt>),
}

impl Display for Expr {
  fn fmt(&self, f : &mut alloc::fmt::Formatter<'_>) -> alloc::fmt::Result {
    match self {
      // Expr::P(_) => todo!(),
      Expr::Lam(bound, body) => write!(f, "λ{}. {}", bound, body),
      Expr::Var(v) => write!(f, "{}", v),
      Expr::App(func, val) => write!(f, "({} {})", func, val),
      Expr::Cons(lhs, rhs) => write!(f, "{{{} {}}}", lhs, rhs),
      Expr::Fst(e) => write!(f, "{}.0", e),
      Expr::Snd(e) => write!(f, "{}.1", e),
      Expr::Match(with, inl, inr) => write!(f, "match {} in {} | {}", with, inl, inr),
      Expr::Inl(e) => write!(f, "{}:0", e),
      Expr::Inr(e) => write!(f, "{}:1", e),
      Expr::Program(p) => todo!(),
      Expr::Word(_) => todo!(),
      Expr::Text(_) => todo!(),
    }
  }
}

// impl From<(Expr, Expr)> for Expr {
//   fn from((a, b) : (Expr, Expr)) -> Self { Expr::Cons(box a, box b) }
// }

// impl From<PValue> for Expr {
//   fn from(v : PValue) -> Self { Expr::P(v.into()) }
// }

// impl From<String> for Expr {
//   fn from(s : String) -> Self { Expr::Var(s) }
// }

impl Expr {
  pub fn subst(self, name : &String, to : &Self) -> Result<Self, Error> {
    use Expr::*;
    match self {
      Text(_) | Word(_) => Ok(self),
      Lam(bound, body) => {
        if *name == bound {
          Ok(Lam(bound, body))
        } else {
          Ok(Lam(bound, box body.subst(name, to)?))
        }
      }
      Var(ref v) => {
        if v == name {
          Ok(to.clone())
        } else {
          Ok(self)
        }
      }
      App(func, val) => Ok(App(box func.subst(name, to)?, box val.subst(name, to)?)),
      Program(v) => Ok(Program(
        v.iter()
          .cloned()
          .map(|s| match s {
            Stmt::Command(_) => Ok(s),
            Stmt::Push(e) => Ok(Stmt::Push(e.subst(name, to)?)),
          })
          .into_iter()
          .collect::<Result<Vec<Stmt>, Error>>()?,
      )),
      Cons(fst, snd) => Ok(Cons(box fst.subst(name, to)?, box snd.subst(name, to)?)),
      Fst(e) | Snd(e) | Inl(e) | Inr(e) => e.subst(name, to),
      Match(with, inl, inr) => Ok(Match(
        box with.subst(name, to)?,
        box inl.subst(name, to)?,
        box inr.subst(name, to)?,
      )),
    }
  }

  pub fn redux(&self) -> bool {
    use Expr::*;
    match self {
      Word(_) | Text(_) | Var(_) | Lam(..) | Program(_) => false,
      App(box Lam(..), _) => true,
      App(func, _) => func.redux(),
      Cons(fst, snd) => fst.redux() || snd.redux(),
      Fst(box Cons(..)) | Snd(box Cons(..)) => true,
      Fst(e) | Snd(e) => e.redux(),
      Match(box Inl(_), ..) | Match(box Inr(_), ..) => true,
      Match(e, ..) | Inl(e) | Inr(e) => e.redux(),
    }
  }

  pub fn red(self) -> Result<Self, Error> {
    use Expr::*;
    // use Primitive::*;

    match self {
      App(box func, box val) => match (func, val) {
        // (P(Fn11(f)), P(V(v))) => Ok(P(V((*f)(v)))),

        // (P(Fn21(f)), Cons(box P(V(v1)), box P(V(v2)))) => Ok(P(V((*f)(v1, v2)))),

        // (P(Fn12(f)), P(V(v))) => {
        //   let (fst, snd) = (*f)(v);
        //   Ok(Program(vec![
        //     Stmt::Push(snd.into()),
        //     Stmt::Push(fst.into()),
        //   ]))
        // }

        // (P(Fn22(f)), Cons(box P(V(v1)), box P(V(v2)))) => {
        //   let (fst, snd) = (*f)(v1, v2);
        //   Ok(Program(vec![
        //     Stmt::Push(snd.into()),
        //     Stmt::Push(fst.into()),
        //   ]))
        // }

        // (P(p), val) => Ok(App(box P(p), box val.red()?)),
        (Lam(ref bound, body), val) => body.subst(bound, &val),

        (func, val) => Ok(App(box func.red()?, box val)),
      },

      Cons(fst, snd) => {
        if fst.redux() {
          Ok(Cons(box fst.red().unwrap(), snd))
        } else {
          Ok(Cons(fst, box snd.red()?))
        }
      }

      Fst(box Cons(this, _)) | Snd(box Cons(_, this)) => Ok(*this),
      Fst(e) | Snd(e) => e.red(),

      Match(box Inl(x), this, _) | Match(box Inr(x), _, this) => Ok(App(this, x)),
      Match(e, inl, inr) => Ok(Match(box e.red()?, inl, inr)),

      Inl(e) | Inr(e) => e.red(),

      _ => {
        if self.redux() {
          panic!("({:?}).redux() true but .red() broken lol", self)
        } else {
          Err(Error::NonRedux(self))
        }
      }
    }
  }

  #[inline]
  pub fn simpl(mut self) -> Self {
    while self.redux() {
      self = self.red().unwrap()
    }
    self
  }
}
