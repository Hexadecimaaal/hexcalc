use alloc::vec;
use alloc::{
  boxed::Box,
  string::{String, ToString},
  vec::Vec,
};

use crate::errors::{Cyclic, Incomplete, Subtype, Unbound};
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

#[derive(Debug, Clone, PartialEq)]
pub enum Condition {
  UVar(String),
  Typing(Expr, Type),
  EVar(String),
  Instantiate(String, Type),
  Mark(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context(pub Vec<Condition>);

impl Context {
  pub fn completed(&self) -> Result<(), Box<Incomplete>> {
    let mut incomplete : Vec<String> = vec![];
    for r in self.0.iter() {
      if let Condition::EVar(ref evar) = *r {
        incomplete.push(evar.to_string());
      }
    }
    if incomplete.is_empty() {
      Ok(())
    } else {
      Err(box Incomplete(incomplete))
    }
  }

  pub fn well_formed(&self, prefix : usize) -> Result<(), Box<dyn CheckingError>> {
    if prefix == 0 {
      return Ok(());
    } else {
      self.well_formed(prefix - 1)?;
      use Condition::*;
      let pivot = &self.0[prefix];
      let head = &self.0[..prefix - 1];
      match pivot {
        UVar(_) | EVar(_) | Mark(_) => {
          for h in head.iter() {
            pivot.no_shadow(h)?;
          }
        }
        Typing(_, ty) | Instantiate(_, ty) => {
          for h in head.iter() {
            pivot.no_shadow(h)?;
          }
          ty.well_formed(head)?;
        }
      }
    }
    Ok(())
  }

  pub fn mentioned(&self, cond : &Condition) -> Result<(), Box<dyn CheckingError>> {
    for c in self.0.iter() {
      if c == cond {
        return Ok(());
      }
    }
    panic!()
  }

  fn find_evar(&self, evar : &str) -> Result<usize, Box<Unbound>> {
    let pivot = self
      .0
      .iter()
      .position(|c| match c {
        Condition::EVar(e) | Condition::Instantiate(e, _) => e == &evar,
        _ => false,
      })
      .ok_or_else(|| box Unbound(Type::EVar(evar.to_string())))?;
    Ok(pivot)
  }

  pub fn solve(&mut self, evar : &str, solution : Type) -> Result<(), Box<dyn CheckingError>> {
    let prefix = self.find_evar(evar)?;
    self.well_formed(prefix)?;
    solution.well_formed(&self.0[..prefix - 1])?;
    self.0[prefix] = Condition::Instantiate(evar.to_string(), solution);
    Ok(())
  }

  pub fn insert(&mut self, cond : Condition, evar : &str) -> Result<(), Box<dyn CheckingError>> {
    let prefix = self.find_evar(evar)?;
    self.0.insert(prefix, cond);
    Ok(())
  }

  pub fn fresh(&self, evar : &str) -> String {
    let mut postfix : u16 = 0;
    while self.0.iter().any(|c| match c {
      Condition::EVar(e) | Condition::Instantiate(e, _) => {
        *e == evar.to_string() + &postfix.to_string()
      }
      _ => false,
    }) {
      postfix = postfix + 1
    }
    evar.to_string() + &postfix.to_string()
  }

  pub fn drop_after(&mut self, cond : &Condition) {
    self
      .0
      .truncate(self.0.iter().position(|c| c == cond).unwrap());
  }
}

impl Type {
  pub fn mono(&self) -> bool {
    match self {
      Type::Forall(..) => false,
      _ => true,
    }
  }

  pub fn instantiate(self, uvar : &str, evar : &str) -> Type {
    use Type::*;
    match self {
      Arrow(box a, box b) => Arrow(box a.instantiate(uvar, evar), box b.instantiate(uvar, evar)),
      Either(box a, box b) => Either(box a.instantiate(uvar, evar), box b.instantiate(uvar, evar)),
      Pair(box a, box b) => Pair(box a.instantiate(uvar, evar), box b.instantiate(uvar, evar)),
      Word | Text | Unit | Empty | EVar(_) => self,
      UVar(ref u) => {
        if u == uvar {
          EVar(evar.to_string())
        } else {
          self
        }
      }
      Forall(u, box a) => {
        if u == uvar {
          Forall(u, box a)
        } else {
          Forall(u, box a.instantiate(uvar, evar))
        }
      }
    }
  }

  pub fn subst(self, ctx : &[Condition]) -> Self {
    use Type::*;
    match self {
      Word | Text | Unit | Empty | UVar(_) => self,
      Arrow(box a, box b) => Arrow(box a.subst(ctx), box b.subst(ctx)),
      Pair(box a, box b) => Pair(box a.subst(ctx), box b.subst(ctx)),
      Either(box a, box b) => Either(box a.subst(ctx), box b.subst(ctx)),
      EVar(evar) => {
        for c in ctx.iter() {
          if let Condition::EVar(def) = c {
            if def == &evar {
              return EVar(evar);
            }
          } else if let Condition::Instantiate(def, ty) = c {
            if def == &evar {
              return ty.clone().subst(ctx);
            }
          }
        }
        panic!()
      }
      Forall(uvar, box ty) => Forall(uvar, box ty.subst(ctx)),
    }
  }

  fn well_formed_(&self, ctx : &[Condition], uvars : &mut Vec<String>) -> Result<(), Box<Unbound>> {
    use Type::*;
    match self {
      Arrow(a, b) | Pair(a, b) | Either(a, b) => {
        a.well_formed_(ctx, uvars)?;
        b.well_formed_(ctx, uvars)
      }
      Word | Text | Unit | Empty => Ok(()),
      UVar(u) => {
        for c in ctx.iter() {
          if let Condition::UVar(uvar) = c {
            if uvar == u {
              return Ok(());
            }
          }
        }
        Err(box Unbound(self.clone()))
      }
      EVar(e) => {
        for c in ctx.iter() {
          match c {
            Condition::EVar(evar) | Condition::Instantiate(evar, _) => {
              if evar == e {
                return Ok(());
              } else {
                continue;
              }
            }
            _ => continue,
          }
        }
        Err(box Unbound(self.clone()))
      }
      Forall(a, b) => {
        uvars.push(a.clone());
        b.well_formed_(ctx, uvars)?;
        uvars.pop();
        Ok(())
      }
    }
  }

  pub fn well_formed(&self, ctx : &[Condition]) -> Result<(), Box<Unbound>> {
    let mut uvars = Vec::<String>::new();
    self.well_formed_(ctx, &mut uvars)
  }

  pub fn free_evar(&self, evar : &str) -> Result<(), Box<Cyclic>> {
    use Type::*;
    match self {
      EVar(s) => {
        if s != evar {
          Ok(())
        } else {
          Err(box Cyclic(EVar(evar.to_string()), self.clone()))
        }
      }
      Word | Text | Unit | Empty | UVar(_) => Ok(()),
      Forall(_, box ty) => ty
        .free_evar(evar)
        .map_err(|box Cyclic(evar, _)| box Cyclic(evar, self.clone())),
      Arrow(box a, box b) | Pair(box a, box b) | Either(box a, box b) => a
        .free_evar(evar)
        .and(b.free_evar(evar))
        .map_err(|box Cyclic(evar, _)| box Cyclic(evar, self.clone())),
    }
  }

  pub fn free_uvar(&self, uvar : &str) -> Result<(), Box<Cyclic>> {
    use Type::*;
    match self {
      UVar(s) => {
        if s != uvar {
          Ok(())
        } else {
          Err(box Cyclic(UVar(uvar.to_string()), self.clone()))
        }
      }
      Word | Text | Unit | Empty | EVar(_) => Ok(()),
      Forall(_, box ty) => ty
        .free_uvar(uvar)
        .map_err(|box Cyclic(evar, _)| box Cyclic(evar, self.clone())),
      Arrow(box a, box b) | Pair(box a, box b) | Either(box a, box b) => a
        .free_uvar(uvar)
        .and(b.free_uvar(uvar))
        .map_err(|box Cyclic(evar, _)| box Cyclic(evar, self.clone())),
    }
  }

  pub fn subtype(self, other : Type, ctx : &mut Context) -> Result<(), Box<dyn CheckingError>> {
    use Type::*;
    match (self, other) {
      (Unit, Unit) => Ok(()),
      (UVar(u), UVar(v)) if u == v => {
        ctx.mentioned(&Condition::UVar(u.clone()))?;
        Ok(())
      }
      (EVar(e), EVar(f)) if e == f => {
        ctx.mentioned(&Condition::EVar(e.clone()))?;
        Ok(())
      }
      (Arrow(box t1, box t2), Arrow(box u1, box u2)) => {
        t1.subtype(u1, ctx)?;
        t2.subst(&ctx.0).subtype(u2.subst(&ctx.0), ctx)?;
        Ok(())
      }
      (Forall(a, box t), u) => {
        let evar = ctx.fresh(&a);
        ctx.0.push(Condition::Mark(evar.clone()));
        ctx.0.push(Condition::EVar(evar.clone()));
        t.instantiate(&a, &evar).subtype(u, ctx)?;
        ctx.drop_after(&Condition::Mark(evar));
        Ok(())
      }
      (t, Forall(a, box u)) => {
        ctx.0.push(Condition::UVar(a.clone()));
        t.subtype(u, ctx)?;
        ctx.drop_after(&Condition::UVar(a));
        Ok(())
      }
      (l, r) => Err(box Subtype(l.clone(), r.clone())),
    }
  }

  pub fn instantiate_super(
    self,
    evar : &str,
    ctx : &mut Context,
  ) -> Result<(), Box<dyn CheckingError>> {
    use Type::*;
    match self {
      Arrow(box t, box u) => {
        ctx.find_evar(evar)?;
        let et = ctx.fresh(evar);
        let eu = ctx.fresh(evar);
        ctx.insert(Condition::EVar(eu.clone()), evar)?;
        ctx.insert(Condition::EVar(et.clone()), evar)?;
        ctx.solve(
          evar,
          Type::Arrow(box Type::EVar(et.clone()), box Type::EVar(eu.clone())),
        )?;
        t.instantiate_sub(&et, ctx)?;
        u.instantiate_super(&eu, ctx)?;
        Ok(())
      }
      Pair(box t, box u) => {
        ctx.find_evar(evar)?;
        let et = ctx.fresh(evar);
        let eu = ctx.fresh(evar);
        ctx.insert(Condition::EVar(eu.clone()), evar)?;
        ctx.insert(Condition::EVar(et.clone()), evar)?;
        ctx.solve(
          evar,
          Type::Pair(box Type::EVar(et.clone()), box Type::EVar(eu.clone())),
        )?;
        t.instantiate_super(&et, ctx)?;
        u.instantiate_super(&eu, ctx)?;
        Ok(())
      }
      Either(box t, box u) => {
        ctx.find_evar(evar)?;
        let et = ctx.fresh(evar);
        let eu = ctx.fresh(evar);
        ctx.insert(Condition::EVar(eu.clone()), evar)?;
        ctx.insert(Condition::EVar(et.clone()), evar)?;
        ctx.solve(
          evar,
          Type::Either(box Type::EVar(et.clone()), box Type::EVar(eu.clone())),
        )?;
        t.instantiate_super(&et, ctx)?;
        u.instantiate_super(&eu, ctx)?;
        Ok(())
      }
      EVar(e) if ctx.find_evar(&e)? > ctx.find_evar(evar)? => {
        ctx.solve(&e, Type::EVar(evar.to_string()))
      }
      Forall(a, box t) => {
        ctx.mentioned(&Condition::EVar(evar.to_string()))?;
        ctx.0.push(Condition::UVar(a.clone()));
        t.instantiate_super(evar, ctx)?;
        ctx.drop_after(&Condition::UVar(a));
        Ok(())
      }
      _ => {
        self.well_formed(&ctx.0)?;
        ctx.solve(evar, self)?;
        Ok(())
      }
    }
  }

  pub fn instantiate_sub(
    self,
    evar : &str,
    ctx : &mut Context,
  ) -> Result<(), Box<dyn CheckingError>> {
    todo!()
  }
}

impl Condition {
  pub fn free(&self, ty : &Type) -> bool {
    use Type::*;
    match ty {
      Word | Text | Unit | Empty | Forall(..) => true,
      Arrow(box a, box b) | Pair(box a, box b) | Either(box a, box b) => {
        self.free(a) && self.free(b)
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

  fn free_uvar(&self, uvar : &str) -> Result<(), Box<Cyclic>> {
    use Condition::*;
    match self {
      UVar(s) => {
        if *s != uvar {
          Ok(())
        } else {
          Err(box Cyclic(
            Type::UVar(uvar.to_string()),
            Type::UVar(s.to_string()),
          ))
        }
      }
      EVar(_) | Mark(_) => Ok(()),
      Instantiate(_, ty) | Typing(_, ty) => ty
        .free_uvar(uvar)
        .map_err(|box Cyclic(uvar, _)| box Cyclic(uvar, (*ty).clone())),
    }
  }

  fn free_evar(&self, evar : &str) -> Result<(), Box<Cyclic>> {
    use Condition::*;
    match self {
      UVar(_) => Ok(()),
      Typing(_, ty) => ty.free_evar(evar),
      EVar(s) | Mark(s) => {
        if *s != evar {
          Ok(())
        } else {
          Err(box Cyclic(
            Type::EVar(evar.to_string()),
            Type::EVar(s.to_string()),
          ))
        }
      }
      Instantiate(s, ty) => ty.free_evar(evar).and(if *s != evar {
        Ok(())
      } else {
        Err(box Cyclic(
          Type::EVar(evar.to_string()),
          Type::EVar(s.to_string()),
        ))
      }),
    }
  }

  pub fn no_shadow(&self, to : &Condition) -> Result<(), Box<Cyclic>> {
    use Condition::*;
    match self {
      UVar(uvar) => to.free_uvar(uvar),
      Typing(..) => Ok(()),
      EVar(evar) | Instantiate(evar, _) | Mark(evar) => to.free_evar(evar),
    }
  }
}
