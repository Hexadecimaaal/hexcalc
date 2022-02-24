use core::borrow::Borrow;
use alloc::{ sync::Arc, string::String, vec::Vec };
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
  Fn11(Arc<dyn Send + Sync + Fn(PValue) -> PValue>),
  Fn21(Arc<dyn Send + Sync + Fn(PValue, PValue) -> PValue>),
  Fn12(Arc<dyn Send + Sync + Fn(PValue) -> (PValue, PValue)>),
  Fn22(Arc<dyn Send + Sync + Fn(PValue, PValue) -> (PValue, PValue)>),
  Fn31(Arc<dyn Send + Sync + Fn(PValue, PValue, PValue) -> PValue>)
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
  Lam{bound : String, body : Arc<Expr>},
  Var(String),
  App{func : Arc<Expr>, val : Arc<Expr>},
  Program(Vec<Stmt>)
}

impl Expr {
  pub fn subst(self : &Arc<Self>, name : &String, to : &Arc<Self>) 
  -> Result<Arc<Self>, Error> {
    match self.borrow() {
      Expr::P(_) => Ok(self.clone()),
      Expr::Lam{bound, body} => 
      if name == bound {Ok(self.clone())} else {
        Ok(Arc::new(Expr::Lam{
          bound : bound.clone(), 
          body : body.subst(name, to)?
        }))
      }
      Expr::Var(v) =>
       if v == name {Ok(to.clone())} else {Ok(self.clone())}
      Expr::App{func, val} => 
        Ok(Arc::new(Expr::App{
          func : func.subst(name, to)?, 
          val : val.subst(name, to)?
        })),
      Expr::Program(v) => Ok(Arc::new(Expr::Program(
        v.iter().cloned().map(|s| match s {
          Stmt::Command(_) => Ok(s),
          Stmt::Push(e) => Ok(Stmt::Push(e.subst(name, to)?))
        }).into_iter().collect::<Result<Vec<Stmt>, Error>>()?
      )))
    }
  }

  pub fn red(self : &Arc<Self>) -> Result<Arc<Self>, Error> {
    use Expr::*;
    use Primitive::*;

    // ---
    // I spent 3 hours on this s**t and gave up. never try to use
    // `fn(..)` instead of `dyn Sync + Send + Fn(..)` ever again
    // ---
    //
    // if let App {func, val} = self.borrow() {
    //   match func.borrow() {
    //     Lam {bound, body} => body.subst(bound, val),
    //     P(Fn11(f)) => 
    //     if let P(V(v)) = val.borrow() {
    //       Ok(Arc::new(P(V(f(*v)))))
    //     } else { Ok(Arc::new(App {func: func.clone(), val: val.red()?})) },
    //     App{func: ifunc, val: ival} =>
    //     //if let P(V(iv)) = ival.borrow() {
    //       match ifunc.borrow() {
    //         P(Fn21(f)) =>
    //         if let P(V(v)) = val.borrow() {
    //         if let P(V(iv)) = ival.borrow() {
    //           Ok(Arc::new(P(V(f(*v, *iv)))))
    //         } else { Ok(Arc::new(App {
    //           func: Arc::new(
    //             App {func: ifunc.clone(), val: ival.red()?}
    //           ),
    //           val: val.clone()
    //         })) }
    //         } else { Ok(Arc::new(App {func: func.clone(), val: val.red()?})) },
    //         App{func: iifunc, val: iival} =>
    //         match iifunc.borrow() {
    //           P(Fn31(f)) =>
    //           if let P(V(v)) = val.borrow() {
    //           if let P(V(iv)) = ival.borrow() {
    //           if let P(V(iiv)) = iival.borrow() {
    //               Ok(Arc::new(P(V(f(*v, *iv, *iiv)))))
    //           } else { Ok(Arc::new(App {
    //             func: Arc::new(App {func: Arc::new( App {
    //               func: iifunc.clone(),
    //               val: iival.red()?
    //             }), val: ival.clone()}),
    //             val: val.clone()
    //           })) }
    //           } else { Ok(Arc::new(App {
    //             func: Arc::new(App {func: ifunc.clone(), val: ival.red()?}),
    //             val: val.clone()
    //           })) }
    //           } else { Ok(Arc::new(App {func: func.clone(), val: val.red()?})) },
    //           _ => panic!()
    //         },
    //         _ => panic!()
    //       }
    //     //} else { Ok(Arc::new(App {func: })) },
    //     _ => panic!()
    //   }
    //   // else if let P(V(v)) = val.borrow() {
    //   //   match func.borrow() {
    
    match self.borrow() {
      App {func, val} => match func.borrow() {
        P(p) => if let P(V(v)) = *val.clone() {
          match p {
            Fn11(f) => 
              Ok(Arc::new(P(V((*f)(v))))),
            Fn21(f) => {
              let ff = f.clone();
              Ok(Arc::new(P(Fn11(
                Arc::new(move |x| ff(v, x))
              ))))
            },
            Fn31(f) => {
              let ff = f.clone();
              Ok(Arc::new(P(Fn21(
                Arc::new(move |x, y| ff(v, x, y))
              ))))
            },
            _ => panic!() 
          }
        } else { Ok(Arc::new(App{func: func.clone(), val: val.red()?})) }
        Lam{bound, body} => body.subst(bound, val),
        _ => Ok(Arc::new(App{func : func.red()?, val: val.clone()}))
      },
      _ => Err(Error::NonRedux(self.clone()))
    }

  }

  #[inline]
  pub fn simpl(self : &Arc<Self>) -> Arc<Self> {
    self.red()
    .map_or_else(|_|self.clone(), |res| res.simpl())
  }
}
