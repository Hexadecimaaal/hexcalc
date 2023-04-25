use crate::{errors::Error, state::Stmt, typing::Type};
use alloc::{boxed::Box, fmt::Display, format, string::String, vec, vec::Vec};
use core::{num::Wrapping, panic};

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

impl core::fmt::Display for Word64 {
  fn fmt(&self, f : &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
    self.format_u().fmt(f)
  }
}

impl PartialEq for Word64 {
  fn eq(&self, rhs : &Self) -> bool { unsafe { self.u == rhs.u } }
}

impl Word64 {
  pub fn format_i(&self) -> String {
    let i = unsafe { self.i };
    if i >= Wrapping(0) {
      format!("0+{:X}", i)
    } else {
      format!("0-{:X}", -i)
    }
  }
  pub fn format_u(&self) -> String { format!("{:#X}", unsafe { self.u }) }
  pub fn format_f(&self) -> String {
    let u = unsafe { self.u }.0;
    let frac = u << 12 >> 12;
    let exp = (u >> 52 & !(1 << 12)) as i16 - 1023;
    let frac_format = format!("{:X}", frac);
    let sig_text = if (u >> 63) != 0 { "0-" } else { "0+" };
    if exp == -1023 {
      format!("{}0.{}", sig_text, frac_format.trim_end_matches('0'))
    } else if exp == 1024 {
      if frac == 0 {
        format!("{}INF", sig_text)
      } else {
        format!("{}{}NAN", sig_text, frac_format.trim_end_matches('0'))
      }
    } else {
      format!(
        "{}1.{}{:+X}",
        sig_text,
        frac_format.trim_end_matches('0'),
        exp
      )
    }
  }
}

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

  Unit,

  Annotated(Box<Expr>, Type),
}

impl Display for Expr {
  fn fmt(&self, f : &mut alloc::fmt::Formatter<'_>) -> alloc::fmt::Result {
    use Expr::*;
    match self {
      Lam(bound, body) => write!(f, "λ{}. {}", bound, body),
      Var(v) => write!(f, "{}", v),
      App(func, val) => write!(f, "({} {})", func, val),
      Cons(lhs, rhs) => write!(f, "{{{} {}}}", lhs, rhs),
      Fst(e) => write!(f, "{}.0", e),
      Snd(e) => write!(f, "{}.1", e),
      Match(with, inl, inr) => write!(f, "match {} with {} | {} end", with, inl, inr),
      Inl(e) => write!(f, "{}:0", e),
      Inr(e) => write!(f, "{}:1", e),
      Program(p) => todo!(),
      Word(w) => w.fmt(f),
      Text(t) => t.fmt(f),
      Unit => write!(f, "*"),
      Annotated(e, t) => todo!(),
    }
  }
}

// impl From<(Expr, Expr)> for Expr {
//   fn from((a, b) : (Expr, Expr)) -> Self { Expr::Cons(box a, box b) }
// }

impl From<Word64> for Expr {
  fn from(v : Word64) -> Self { Expr::Word(v) }
}

impl From<i64> for Expr {
  fn from(v : i64) -> Self { Expr::Word(Word64 { i : Wrapping(v) }) }
}

impl From<u64> for Expr {
  fn from(v : u64) -> Self { Expr::Word(Word64 { u : Wrapping(v) }) }
}

impl From<f64> for Expr {
  fn from(v : f64) -> Self { Expr::Word(Word64 { f : v }) }
}

impl From<Stmt> for Expr {
  fn from(s : Stmt) -> Self {
    match s {
      Stmt::Push(e) => e,
      _ => Expr::Program(vec![s]),
    }
  }
}

// impl From<String> for Expr {
//   fn from(s : String) -> Self { Expr::Var(s) }
// }

impl Expr {
  pub fn subst(self, name : &str, to : &Self) -> Result<Self, Error> {
    use Expr::*;
    match self {
      Text(_) | Word(_) | Unit => Ok(self),
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
        v.into_iter()
          .map(|s| match s {
            Stmt::Command(_) => Ok(s),
            Stmt::Push(e) => Ok(Stmt::Push(e.subst(name, to)?)),
          })
          .collect::<Result<Vec<Stmt>, Error>>()?,
      )),
      Cons(fst, snd) => Ok(Cons(box fst.subst(name, to)?, box snd.subst(name, to)?)),
      Fst(e) | Snd(e) | Inl(e) | Inr(e) | Annotated(e, _) => e.subst(name, to),
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
      Word(_) | Text(_) | Var(_) | Lam(..) | Program(_) | Unit => false,
      App(box Lam(..), _) => true,
      App(func, _) => func.redux(),
      Cons(fst, snd) => fst.redux() || snd.redux(),
      Fst(box Cons(..)) | Snd(box Cons(..)) => true,
      Fst(e) | Snd(e) => e.redux(),
      Match(box Inl(_), ..) | Match(box Inr(_), ..) => true,
      Match(e, ..) | Inl(e) | Inr(e) | Annotated(e, _) => e.redux(),
    }
  }

  pub fn red(self) -> Result<Self, Error> {
    use Expr::*;

    match self {
      App(box func, box val) => match (func, val) {
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
      Fst(e) => Ok(Fst(box e.red()?)),
      Snd(e) => Ok(Snd(box e.red()?)),

      Match(box Inl(x), this, _) | Match(box Inr(x), _, this) => Ok(App(this, x)),
      Match(e, inl, inr) => Ok(Match(box e.red()?, inl, inr)),

      Inl(e) => Ok(Inl(box e.red()?)),
      Inr(e) => Ok(Inr(box e.red()?)),

      Annotated(e, t) => Ok(e.red()?),

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
