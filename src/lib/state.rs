use alloc::{ 
  collections::{ BTreeMap },
  vec::Vec,
  string::String
};
use crate::{ expr::Expr, errors::Error };

#[derive(Debug, Clone)]
pub struct Stack(Vec<Expr>);

impl Stack {
  #[inline] pub fn pop(&mut self) -> Result<Expr, Error> {
    self.0.pop().ok_or(Error::EmptyStack(1))
  }

  #[inline] pub fn push(&mut self, e : Expr) {
    self.0.push(e)
  }

  #[inline] pub fn dup(&mut self) -> Result<(), Error> {
    let e = self.pop()?;
    self.push(e.clone());
    self.push(e);
    Ok(())
  }

  #[inline] pub fn swap(&mut self) -> Result<(), Error> {
    let err  = Err(Error::EmptyStack(2));
    let e = 
      self.pop().or(err.clone())?;
    let f = 
      self.pop().or(err)?;
    self.push(e);
    self.push(f);
    Ok(())
  }

  #[inline] pub fn pop_n(&mut self, n : usize) 
  -> Result<Vec<Expr>, Error> {
    let err  = Err(Error::EmptyStack(n));
    if n > self.0.len() { err } else {
      Ok(self.0.split_off(self.0.len() - n))
    }
  }

  #[inline] pub fn push_n(&mut self, mut e : Vec<Expr>) {
    self.0.append(&mut e);
  }

  #[inline] pub fn rotate(&mut self, n : usize, amount : usize) 
  -> Result<(), Error> {
    let mut part = self.pop_n(n)?;
    part.rotate_right(amount % n);
    self.push_n(part);
    Ok(())
  }

  #[inline] pub fn rotate_down(&mut self, n : usize, amount : usize)
  -> Result<(), Error> {
    let mut part = self.pop_n(n)?;
    part.rotate_left(amount % n);
    self.push_n(part);
    Ok(())
  }

  #[inline] pub fn roll(&mut self, n : usize) -> Result<(), Error> { 
    self.rotate(n, 1)
  }

  #[inline] pub fn rot(&mut self) -> Result<(), Error> {
    self.roll(3)
  }

  #[inline] pub fn rolld(&mut self, n : usize) -> Result<(), Error> {
    self.rotate_down(n, 1)
  }

  #[inline] pub fn at(&self, n : usize) -> Result<Expr, Error> {
    let err = Err(Error::EmptyStack(n));
    if n > self.0.len() { err } else {
      Ok(self.0[self.0.len() - n].clone())
    }
  }

  #[inline] pub fn pick(&mut self, n : usize) -> Result<(), Error> {
    self.push(self.at(n)?);
    Ok(())
  }

  #[inline] pub fn over(&mut self) -> Result<(), Error> {
    self.pick(2)
  }
}



#[derive(Debug, Clone)]
pub struct Context(BTreeMap<String, Expr>);

impl Context {
  #[inline] pub fn get(&self, name : &String) -> Result<Expr, Error> {
    self.0.get(name).ok_or(Error::Undefined(name.clone()))
      .map(|arc| arc.clone())
  }

  #[inline] pub fn sto(&mut self, name : &String, val : Expr)
  -> Option<Expr>{
    self.0.insert(name.clone(), val)
  }

  #[inline] pub fn try_sto(&mut self, name : &String, val : Expr)
  -> Result<(), Error> {
    if self.0.contains_key(name) { Err(Error::Exists(name.clone())) }
    else { self.0.insert(name.clone(), val); Ok(()) }
  }

  #[inline] pub fn purge(&mut self, name : &String) 
  -> Result<Expr, Error> {
    self.0.remove(name).ok_or(Error::Undefined(name.clone()))
  }
}


#[derive(Clone)]
pub enum Stmt {
  Push(Expr),
  Command(&'static fn(&mut State) -> Result<(), Error>)
}

impl core::fmt::Debug for Stmt {
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>)
  -> Result<(), core::fmt::Error> { match self {
    Stmt::Push(e) => write!(f, "{:?}", e)?,
    _ => write!(f, "{:?}", self as *const Stmt as usize)?
  } Ok(())}
}

impl PartialEq for Stmt {
  fn eq(&self, rhs: &Self) -> bool { 
    use Stmt::*;
    match (self, rhs) {
      (Push(a), Push(b)) => a == b,
      (Command(a), Command(b)) => (a as *const _) == (b as *const _),
      _ => false
    }
  }
}

#[derive(Debug, Clone)]
pub struct State(Stack, Context);

impl State {
  pub fn eval(&mut self, c : Stmt) -> Result<(), Error> {
    use Stmt::*;
    match c {
      Push(e) => Ok(self.0.push(e)),
      Command(c) => c(self)
    }
  }

  pub fn apply(&mut self) -> Result<(), Error> {
    match self.0.pop()? {
      Expr::Program(p) => Ok(for i in p {
        self.eval(i)?
      }),
      fun => {
        let top = self.0.pop()?;
        self.eval(Stmt::Push(Expr::App(box fun, box top)))
      }
    }
  }
}
