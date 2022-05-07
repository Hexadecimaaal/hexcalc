use crate::{errors::Error, expr::Expr};
use alloc::{collections::BTreeMap, string::String, vec::Vec};

#[derive(Debug, Clone)]
pub struct Stack(pub Vec<Expr>);

impl Stack {
  #[inline]
  pub fn pop(&mut self) -> Result<Expr, Error> { self.0.pop().ok_or(Error::EmptyStack(1)) }

  #[inline]
  pub fn push(&mut self, e : Expr) { self.0.push(e) }

  #[inline]
  pub fn dup(&mut self) -> Result<(), Error> {
    let e = self.pop()?;
    self.push(e.clone());
    self.push(e);
    Ok(())
  }

  #[inline]
  pub fn swap(&mut self) -> Result<(), Error> {
    let err = Err(Error::EmptyStack(2));
    let e = self.pop().or(err.clone())?;
    let f = self.pop().or(err)?;
    self.push(e);
    self.push(f);
    Ok(())
  }

  #[inline]
  pub fn pop_n(&mut self, n : usize) -> Result<Vec<Expr>, Error> {
    let err = Err(Error::EmptyStack(n));
    if n > self.0.len() {
      err
    } else {
      Ok(self.0.split_off(self.0.len() - n))
    }
  }

  #[inline]
  pub fn push_n(&mut self, mut e : Vec<Expr>) { self.0.append(&mut e); }

  #[inline]
  pub fn rotate(&mut self, n : usize, amount : usize) -> Result<(), Error> {
    let mut part = self.pop_n(n)?;
    part.rotate_right(amount % n);
    self.push_n(part);
    Ok(())
  }

  #[inline]
  pub fn rotate_down(&mut self, n : usize, amount : usize) -> Result<(), Error> {
    let mut part = self.pop_n(n)?;
    part.rotate_left(amount % n);
    self.push_n(part);
    Ok(())
  }

  #[inline]
  pub fn roll(&mut self, n : usize) -> Result<(), Error> { self.rotate(n, 1) }

  #[inline]
  pub fn rot(&mut self) -> Result<(), Error> { self.roll(3) }

  #[inline]
  pub fn rolld(&mut self, n : usize) -> Result<(), Error> { self.rotate_down(n, 1) }

  #[inline]
  pub fn at(&self, n : usize) -> Result<Expr, Error> {
    if n > self.0.len() {
      Err(Error::EmptyStack(n))
    } else if n <= 0 {
      Err(Error::Range(0, self.0.len()))
    } else {
      Ok(self.0[self.0.len() - n].clone())
    }
  }

  #[inline]
  pub fn pick(&mut self, n : usize) -> Result<(), Error> {
    self.push(self.at(n)?);
    Ok(())
  }

  #[inline]
  pub fn over(&mut self) -> Result<(), Error> { self.pick(2) }
}

#[derive(Clone)]
pub enum Stmt {
  Push(Expr),
  Command(&'static fn(&mut State) -> Result<(), Error>),
}

impl core::fmt::Debug for Stmt {
  fn fmt(&self, f : &mut core::fmt::Formatter<'_>) -> Result<(), core::fmt::Error> {
    match self {
      Stmt::Push(e) => write!(f, "{:?}", e)?,
      Stmt::Command(e) => write!(f, "#(command at {:?})", *e as *const _ as usize)?,
    }
    Ok(())
  }
}

impl PartialEq for Stmt {
  fn eq(&self, rhs : &Self) -> bool {
    use Stmt::*;
    match (self, rhs) {
      (Push(a), Push(b)) => a == b,
      (Command(a), Command(b)) => (a as *const _) == (b as *const _),
      _ => false,
    }
  }
}

#[derive(Debug, Clone)]
pub struct Variables(pub BTreeMap<String, Stmt>);

impl Variables {
  #[inline]
  pub fn get(&self, name : &str) -> Result<Stmt, Error> {
    self
      .0
      .get(name)
      .ok_or(Error::Undefined(name.into()))
      .map(|arc| arc.clone())
  }

  #[inline]
  pub fn sto(&mut self, name : &str, val : Stmt) -> Option<Stmt> { self.0.insert(name.into(), val) }

  #[inline]
  pub fn try_sto(&mut self, name : &str, val : Stmt) -> Result<(), Error> {
    if self.0.contains_key(name) {
      Err(Error::Exists(name.into()))
    } else {
      self.0.insert(name.into(), val);
      Ok(())
    }
  }

  #[inline]
  pub fn purge(&mut self, name : &str) -> Result<Stmt, Error> {
    self.0.remove(name).ok_or(Error::Undefined(name.into()))
  }
}

#[derive(Debug, Clone)]
pub struct State(pub Stack, pub Variables);

impl State {
  pub fn eval(&mut self, c : Stmt) -> Result<(), Error> {
    use Stmt::*;
    match c {
      Push(e) => Ok(self.0.push(e)),
      Command(c) => c(self),
    }
  }

  pub fn eval_prog(&mut self, p : Vec<Stmt>) -> Result<(), Error> {
    Ok(for i in p {
      self.eval(i)?
    })
  }

  pub fn apply(&mut self) -> Result<(), Error> {
    match self.0.pop()? {
      Expr::Program(p) => self.eval_prog(p),
      fun => {
        let top = self.0.pop()?;
        self.eval(Stmt::Push(Expr::App(box fun, box top)))
      }
    }
  }

  pub fn drop(&mut self) -> Result<(), Error> { self.0.pop().map(|_| ()) }
  pub fn dup(&mut self) -> Result<(), Error> { self.0.dup() }
  pub fn swap(&mut self) -> Result<(), Error> { self.0.swap() }
  pub fn dropn(&mut self) -> Result<(), Error> {
    match self.0.pop()? {
      Expr::Word(w) => self.0.pop_n(unsafe { w.u.0 as usize }).map(|_| ()),
      e => panic!("dropn called with ({:?})", e),
    }
  }
  pub fn roll(&mut self) -> Result<(), Error> {
    match self.0.pop()? {
      Expr::Word(w) => self.0.roll(unsafe { w.u.0 as usize }).map(|_| ()),
      e => panic!("roll called with ({:?})", e),
    }
  }
  pub fn rolld(&mut self) -> Result<(), Error> {
    match self.0.pop()? {
      Expr::Word(w) => self.0.rolld(unsafe { w.u.0 as usize }).map(|_| ()),
      e => panic!("rolld called with ({:?})", e),
    }
  }
  pub fn rot(&mut self) -> Result<(), Error> { self.0.rot() }
  pub fn pick(&mut self) -> Result<(), Error> {
    match self.0.pop()? {
      Expr::Word(w) => self.0.pick(unsafe { w.u.0 as usize }).map(|_| ()),
      e => panic!("pick called with ({:?})", e),
    }
  }
  pub fn over(&mut self) -> Result<(), Error> { self.0.over() }
  pub fn sto(&mut self) -> Result<(), Error> {
    if self.0 .0.len() < 2 {
      Err(Error::EmptyStack(2))
    } else {
      match self.0.pop()? {
        Expr::Text(name) => {
          self.1.sto(&name, Stmt::Push(self.0.pop()?));
          Ok(())
        }
        e => panic!("sto called with ({:?})", e),
      }
    }
  }
  pub fn exch(&mut self) -> Result<(), Error> {
    if self.0 .0.len() < 2 {
      Err(Error::EmptyStack(2))
    } else {
      match self.0.pop()? {
        Expr::Text(name) => {
          if self.1 .0.contains_key(&name) {
            let old = self.1.sto(&name, Stmt::Push(self.0.pop()?)).unwrap();
            Ok(self.0.push(old.into()))
          } else {
            Err(Error::Undefined(name))
          }
        }
        e => panic!("exch called with ({:?})", e),
      }
    }
  }
  pub fn rcl(&mut self) -> Result<(), Error> {
    match self.0.pop()? {
      Expr::Text(t) => self.1.get(&t).map(|res| (self.0.push(res.into()))),
      e => panic!("rcl called with ({:?})", e),
    }
  }
  pub fn purge(&mut self) -> Result<(), Error> {
    match self.0.pop()? {
      Expr::Text(t) => self.1.purge(&t).map(|_| ()),
      e => panic!("purge called with ({:?})", e),
    }
  }
}
