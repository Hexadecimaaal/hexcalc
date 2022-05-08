use alloc::vec;
use alloc::{
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

use crate::errors::{Cyclic, Incomplete};
use crate::{errors::CheckingError, expr::Expr};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
  Arrow(Box<Type>, Box<Type>),
  Word,
  Text,
  Unit,
  Empty,
  Pair(Box<Type>, Box<Type>),
  Either(Box<Type>, Box<Type>),

  UVar(String),
  EVar(String),
  Forall(String, Box<Type>),
}

impl Type {
  pub fn mono(&self) -> bool {
    match self {
      Type::Forall(..) => false,
      _ => true,
    }
  }

  pub fn subst(self, ctx : &Context) -> Self {
    use Type::*;
    match self {
      Word | Text | Unit | Empty | UVar(_) => self,
      Arrow(box a, box b) => Arrow(box a.subst(ctx), box b.subst(ctx)),
      Pair(box a, box b) => Pair(box a.subst(ctx), box b.subst(ctx)),
      Either(box a, box b) => Either(box a.subst(ctx), box b.subst(ctx)),
      EVar(evar) => {
        for c in ctx.0.iter() {
          if let Condition::EVar(def) = c {
            if def == &evar {
              return EVar(evar);
            }
          } else if let Condition::Instantiate(def, ty) = c {
            if def == &evar {
              return (*ty).clone().subst(ctx);
            }
          }
        }
        panic!()
      }
      Forall(uvar, box ty) => Forall(uvar, box ty.subst(ctx)),
    }
  }

  // pub fn free_in(&self, ty : &Type) -> bool {
  //   use Type::*;
  //   match ty {
  //     Word | Text | Unit | Empty => true,
  //     Arrow(box a, box b) | Pair(box a, box b) | Either(box a, box b) => {
  //       self.free_in(a) && self.free_in(b)
  //     }
  //     UVar(uvar) => {
  //       if let UVar(x) = self {
  //         uvar != x
  //       } else {
  //         true
  //       }
  //     }
  //     EVar(evar) => {
  //       if let EVar(x) = self {
  //         evar != x
  //       } else {
  //         true
  //       }
  //     }
  //     Forall(_, box ty) => self.free_in(ty),
  //   }
  // }

  pub fn free_evar(&self, evar : &str) -> Result<(), Cyclic> {
    use Type::*;
    match self {
      EVar(s) => {
        if s != evar {
          Ok(())
        } else {
          Err(Cyclic(EVar(evar.to_string()), self.clone()))
        }
      }
      Word | Text | Unit | Empty | UVar(_) => Ok(()),
      Forall(_, box ty) => ty
        .free_evar(evar)
        .map_err(|Cyclic(evar, _)| Cyclic(evar, self.clone())),
      Arrow(box a, box b) | Pair(box a, box b) | Either(box a, box b) => a
        .free_evar(evar)
        .and(b.free_evar(evar))
        .map_err(|Cyclic(evar, _)| Cyclic(evar, self.clone())),
    }
  }

  pub fn free_uvar(&self, uvar : &str) -> Result<(), Cyclic> {
    use Type::*;
    match self {
      UVar(s) => {
        if s != uvar {
          Ok(())
        } else {
          Err(Cyclic(UVar(uvar.to_string()), self.clone()))
        }
      }
      Word | Text | Unit | Empty | EVar(_) => Ok(()),
      Forall(_, box ty) => ty
        .free_uvar(uvar)
        .map_err(|Cyclic(evar, _)| Cyclic(evar, self.clone())),
      Arrow(box a, box b) | Pair(box a, box b) | Either(box a, box b) => a
        .free_uvar(uvar)
        .and(b.free_uvar(uvar))
        .map_err(|Cyclic(evar, _)| Cyclic(evar, self.clone())),
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Condition<'a> {
  UVar(&'a str),
  Typing(&'a Expr, &'a Type),
  EVar(&'a str),
  Instantiate(&'a str, &'a Type),
  Mark(&'a str),
}

impl Condition<'_> {
  pub fn fresh(&self, ty : &Type) -> bool {
    use Type::*;
    match ty {
      Word | Text | Unit | Empty | Forall(..) => true,
      Arrow(box a, box b) | Pair(box a, box b) | Either(box a, box b) => {
        self.fresh(a) && self.fresh(b)
      }
      UVar(s) => match self {
        Condition::UVar(def) => def != s,
        Condition::Typing(..)
        | Condition::Instantiate(..)
        | Condition::EVar(_)
        | Condition::Mark(_) => true,
      },
      EVar(s) => match self {
        Condition::UVar(_) | Condition::Typing(..) | Condition::Mark(_) => true,
        Condition::EVar(def) | Condition::Instantiate(def, _) => def != s,
      },
    }
  }

  fn free_uvar(&self, uvar : &str) -> Result<(), Cyclic> {
    use Condition::*;
    match self {
      UVar(s) => {
        if *s != uvar {
          Ok(())
        } else {
          Err(Cyclic(
            Type::UVar(uvar.to_string()),
            Type::UVar(s.to_string()),
          ))
        }
      }
      EVar(_) | Mark(_) => Ok(()),
      Instantiate(_, ty) | Typing(_, ty) => ty
        .free_uvar(uvar)
        .map_err(|Cyclic(uvar, _)| Cyclic(uvar, (*ty).clone())),
    }
  }

  fn free_evar(&self, evar : &str) -> Result<(), Cyclic> {
    use Condition::*;
    match self {
      UVar(_) => Ok(()),
      Typing(_, ty) => ty.free_evar(evar),
      EVar(s) | Mark(s) => {
        if *s != evar {
          Ok(())
        } else {
          Err(Cyclic(
            Type::EVar(evar.to_string()),
            Type::EVar(s.to_string()),
          ))
        }
      }
      Instantiate(s, ty) => ty.free_evar(evar).and(if *s != evar {
        Ok(())
      } else {
        Err(Cyclic(
          Type::EVar(evar.to_string()),
          Type::EVar(s.to_string()),
        ))
      }),
    }
  }

  pub fn no_shadow(&self, to : &Condition) -> Result<(), Cyclic> {
    use Condition::*;
    match self {
      UVar(uvar) => to.free_uvar(uvar),
      Typing(..) => Ok(()),
      EVar(evar) | Instantiate(evar, _) | Mark(evar) => to.free_evar(evar),
    }
  }
}

pub fn prefixes<T>(slice : &[T]) -> impl Iterator<Item = &[T]> + DoubleEndedIterator {
  (0..=slice.len()).map(move |len| &slice[..len])
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context<'a>(pub Vec<Condition<'a>>);

impl Context<'_> {
  pub fn completed(&self) -> Result<(), Incomplete> {
    let mut incomplete : Vec<String> = vec![];
    for r in self.0.iter() {
      if let Condition::EVar(evar) = r {
        incomplete.push(evar.to_string());
      }
    }
    if incomplete.is_empty() {
      Ok(())
    } else {
      Err(Incomplete(incomplete))
    }
  }

  pub fn well_formed(&self) -> Result<(), impl CheckingError> {
    for c in prefixes(&self.0) {
      if c.len() == 0 {
        continue;
      } else {
        use Condition::*;
        let pivot = c.last().unwrap();
        let head = &c[..c.len() - 1];
        match pivot {
          UVar(_) | EVar(_) | Mark(_) => {
            for h in head.iter() {
              let r = pivot.no_shadow(h);
              if r.is_err() {
                return r;
              }
            }
          }
          Typing(_, ty) | Instantiate(_, ty) => {
            for h in head.iter() {
              let r = pivot.no_shadow(h);
              if r.is_err() {
                return r;
              }
            }
          }
        }
      }
    }
    Ok(())
  }
}
