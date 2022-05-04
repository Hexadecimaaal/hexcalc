#[cfg(test)]
#[allow(non_snake_case)]
mod expr {
  use crate::expr::*;
  use crate::state::Stmt;
  use Expr::*;
  use Primitive::*;

  fn iplus(lhs : PValue, rhs : PValue) -> PValue { unsafe { PValue { i : lhs.i + rhs.i } } }

  fn idivmod(lhs : PValue, rhs : PValue) -> (PValue, PValue) {
    unsafe { (PValue { i : lhs.i / rhs.i }, PValue { i : lhs.i % rhs.i }) }
  }

  #[test]
  fn expr_eval() {
    let A : Box<Expr> = box Var("a".to_string());
    let B : Box<Expr> = box Var("b".to_string());
    let X : Box<Expr> = box Var("x".to_string());
    let Y : Box<Expr> = box Var("y".to_string());

    let EXPR0 : Box<Expr> = box Lam("y".into(), X.clone());
    let EXPR1 : Box<Expr> = box Lam("x".into(), EXPR0.clone()); // \x. \y. x

    let EXPR2 : Box<Expr> = box App(EXPR1.clone(), A.clone()); // (\x. \y. x) a
    let EXPR3 : Box<Expr> = box App(EXPR1.clone(), B.clone()); // (\x. \y. x) b
    let EXPR4 : Box<Expr> = box App(EXPR2.clone(), B.clone()); // (\x. \y. x) a b

    let IPLUS : Box<Expr> = box P(Fn21(&(iplus as fn(_, _) -> _)));
    let IZERO : Box<Expr> = box PValue { i : 0 }.into();
    let IONE : Box<Expr> = box PValue { i : 1 }.into();
    let ITWO : Box<Expr> = box PValue { i : 2 }.into();
    let IDIVMOD : Box<Expr> = box P(Fn22(&(idivmod as fn(_, _) -> (_, _))));
    let IN15 : Box<Expr> = box PValue { i : 15 }.into();
    let IN31 : Box<Expr> = box PValue { i : 31 }.into();

    let IONE_PLUS_ONE : Box<Expr> = box App(IPLUS.clone(), box Cons(IONE.clone(), IONE.clone()));
    let IONE_PLUS_ZERO : Box<Expr> = box App(IPLUS.clone(), box Cons(IONE.clone(), IZERO.clone()));

    let IN31_DIVMOD_TWO : Box<Expr> =
      box App(IDIVMOD.clone(), box Cons(IN31.clone(), ITWO.clone()));
    let PRGM_IONE_IN15 : Box<Expr> =
      box Program(vec![Stmt::Push(*IONE.clone()), Stmt::Push(*IN15.clone())]);

    assert_eq!(EXPR2.clone().subst(&"a".to_string(), &B).unwrap(), *EXPR3);
    assert_eq!(EXPR2.clone().subst(&"x".to_string(), &Y).unwrap(), *EXPR2);

    assert_eq!(EXPR4.clone().red().unwrap().red().unwrap(), *A);

    assert_eq!(EXPR4.simpl(), *A);
    assert_eq!(IONE_PLUS_ONE.simpl(), *ITWO);
    assert_eq!(IONE_PLUS_ZERO.simpl(), *IONE);
    assert_eq!(IN31_DIVMOD_TWO.simpl(), *PRGM_IONE_IN15)
  }
}
