#[cfg(test)]
mod expr {

  use crate::expr::*;
  use Expr::*;
  use Primitive::*;
  use alloc::sync::Arc;

  fn iplus(lhs : PValue, rhs : PValue) -> PValue {
    unsafe { PValue{ i : lhs.i + rhs.i } }
  }

  lazy_static::lazy_static! {
    pub static ref A : Arc<Expr> = Arc::new(Var("a".to_string()));
    pub static ref B : Arc<Expr> = Arc::new(Var("b".to_string()));
    pub static ref X : Arc<Expr> = Arc::new(Var("x".to_string()));
    pub static ref Y : Arc<Expr> = Arc::new(Var("y".to_string()));

    pub static ref EXPR0 : Arc<Expr> = Arc::new(Lam { // \y. x
      bound : "y".to_string(),
      body : X.clone()
    });

    pub static ref EXPR1 : Arc<Expr> = Arc::new(Lam { // \x. \y. x
      bound : "x".to_string(),
      body : EXPR0.clone()
    });

    pub static ref EXPR2 : Arc<Expr> = Arc::new(App { // (\x. \y. x) a
      func : EXPR1.clone(),
      val : A.clone()
    });

    pub static ref EXPR3 : Arc<Expr> = Arc::new(App { // (\x. \y. x) b
      func : EXPR1.clone(),
      val : B.clone()
    });

    pub static ref EXPR4 : Arc<Expr> = Arc::new(App { // (\x. \y. x) a b
      func : EXPR2.clone(),
      val : B.clone()
    });

    pub static ref IPLUS : Arc<Expr> =
      Arc::new(P(Fn21(Arc::new(iplus))));

    pub static ref IZERO : Arc<Expr> =
      Arc::new(P(V(PValue {i : 0})));

    pub static ref IONE : Arc<Expr> =
      Arc::new(P(V(PValue {i : 1})));

    pub static ref ITWO : Arc<Expr> =
      Arc::new(P(V(PValue {i : 2})));

    pub static ref IONE_PLUS : Arc<Expr> = Arc::new(App {
      func : IPLUS.clone(),
      val : IONE.clone()
    });

    pub static ref IONE_PLUS_ONE : Arc<Expr> = Arc::new(App {
      func : IONE_PLUS.clone(),
      val : IONE.clone()
    });

    pub static ref IONE_PLUS_ZERO : Arc<Expr> = Arc::new(App {
      func : IONE_PLUS.clone(),
      val : IZERO.clone()
    });

  }

  #[test]
  fn subst() {
    assert_eq!(*EXPR2.subst(&"a".to_string(), &B).unwrap(), **EXPR3);
    assert_eq!(*EXPR2.subst(&"x".to_string(), &Y).unwrap(), **EXPR2);
  }

  #[test]
  fn red() {
    assert_eq!(*EXPR4.red().unwrap().red().unwrap(), *A.clone());
  }

  #[test]
  fn simpl() {
    assert_eq!(*EXPR4.simpl(), *A.clone());
  }

  #[test]
  fn primitive() {
    assert_eq!(*IONE_PLUS_ONE.simpl(), *ITWO.clone())
  }
}
