use core::borrow::Borrow;
use alloc::{ boxed::Box, string::String, vec::Vec, vec };
use crate::{ errors::Error, state::Stmt };

#[derive(Clone, Copy)]
pub union PValue {
  pub i : i64,
  pub u : u64,
  pub f : f64
}

impl core::fmt::Debug for PValue {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) 
  -> Result<(), core::fmt::Error> { unsafe {
    core::fmt::Debug::fmt(&self.u, f)
  }}
}

impl PartialEq for PValue {
  fn eq(&self, rhs: &Self) -> bool { unsafe {
    self.u == rhs.u
  }}
}



#[derive(Clone)]
pub enum Primitive {
  V(PValue),
  Fn11(&'static fn(PValue) -> PValue),
  Fn21(&'static fn(PValue, PValue) -> PValue),
  Fn12(&'static fn(PValue) -> (PValue, PValue)),
  Fn22(&'static fn(PValue, PValue) -> (PValue, PValue)),
  Fn31(&'static fn(PValue, PValue, PValue) -> PValue)
}

impl From<PValue> for Primitive {
  fn from(v : PValue) -> Self { Primitive::V(v) }
}

impl core::fmt::Debug for Primitive {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>)
  -> Result<(), core::fmt::Error> { match self {
    Primitive::V(v) => write!(f, "{:?}", v)?,
    _ => write!(f, "{:?}", self.borrow() as *const Primitive as usize)?
  } Ok(())}
}

impl core::cmp::PartialEq for Primitive {
fn eq(&self, rhs: &Primitive) -> bool { match (self, rhs) {
  (Primitive::V(v1), Primitive::V(v2)) => v1 == v2,
  _ => self as *const _ == rhs as *const _
}}}


#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
  P(Primitive),
  Lam(String, Box<Expr>),
  Var(String),
  App(Box<Expr>, Box<Expr>),
  Cons(Box<Expr>, Box<Expr>), Fst, Snd,
  Program(Vec<Stmt>)
}

impl From<(Expr, Expr)> for Expr {
  fn from((a, b): (Expr, Expr)) -> Self {
    Expr::Cons(box a, box b)
  }
}

impl From<Primitive> for Expr {
  fn from(p : Primitive) -> Self {Expr::P(p)}
}

impl From<PValue> for Expr {
  fn from(v : PValue) -> Self {Expr::P(v.into())}
}

impl From<String> for Expr {
  fn from(s : String) -> Self {Expr::Var(s)}
}

impl Expr {
  pub fn subst(self, name : &String, to : &Self) 
  -> Result<Self, Error> {
    use Expr::*;
    match self {
      P(_) | Fst | Snd => Ok(self),
      Lam(bound, body) => 
      if *name == bound {Ok(Lam(bound, body))} else {
        Ok(Lam(bound, box body.subst(name, to)?))
      }
      Var(ref v) =>
       if v == name {Ok(to.clone())} else {Ok(self)}
      App(func, val) => 
        Ok(App(
          box func.subst(name, to)?, 
          box val.subst(name, to)?
        )),
      Program(v) => Ok(Program(
        v.iter().cloned().map(|s| match s {
          Stmt::Command(_) => Ok(s),
          Stmt::Push(e) => Ok(Stmt::Push(e.subst(name, to)?))
        }).into_iter().collect::<Result<Vec<Stmt>, Error>>()?
      )),
      Cons(fst, snd) => Ok(Cons(
        box fst.subst(name, to)?, 
        box snd.subst(name, to)?
      )),
    }
  }

  pub fn redux(&self) -> bool {
    use Expr::*;
    match self {
        P(_) | Fst | Snd | Var(_) | Lam(_, _) => false,
        App(box Lam(_, _), _) => true,
        App(func, _) => func.redux(),
        Cons(fst, snd) => fst.redux() || snd.redux(),
        Program(_) => todo!(),
    }
  }

  pub fn red(self) -> Result<Self, Error> {
    use Expr::*;
    use Primitive::*;
    
    match self {
      App(box func, box val) => match (func, val) {
        (P(Fn11(f)), P(V(v))) => Ok(P(V((*f)(v)))),

        (P(Fn21(f)), Cons(
          box P(V(v1)), box P(V(v2)))) => {
          Ok(P(V((*f)(v1, v2))))
        },

        (P(Fn12(f)), P(V(v))) => {
          let (fst, snd) = (*f)(v);
          Ok(Program(vec![
            Stmt::Push(snd.into()), 
            Stmt::Push(fst.into())]))
        },

        (P(Fn22(f)), Cons(
          box P(V(v1)), box P(V(v2)))) => {
          let (fst, snd) = (*f)(v1, v2);
          Ok(Program(vec![
            Stmt::Push(snd.into()), 
            Stmt::Push(fst.into())]))
        },

        (P(p), val) => Ok(App(box P(p), box val.red()?)),

        (Lam(ref bound, body), val) => body.subst(bound, &val),

        (Fst, Cons(box fst, _)) => Ok(fst),
        (Snd, Cons(_, box snd)) => Ok(snd),

        (func, val) => Ok(App(box func.red()?, box val))
      },

      Cons(fst, snd) => if fst.redux() {
        Ok(Cons(box fst.red().unwrap(), snd))
      } else { Ok(Cons(fst, box snd.red()?)) },

      _ => Err(Error::NonRedux(self)),
    }

  }

  #[inline]
  pub fn simpl(self : Self) -> Self {
    let old = self.clone();
    self.red().map_or(old, |res| res.simpl())
  }
}
