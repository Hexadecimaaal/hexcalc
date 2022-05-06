#[cfg(test)]
#[allow(non_snake_case)]
mod expr {
  use crate::expr::*;
  use crate::state::Stmt;
  use alloc::boxed::Box;
  use alloc::string::ToString;
  use alloc::vec;
  use Expr::*;
  // use Primitive::*;

  fn idivmod(lhs : Word64, rhs : Word64) -> (Word64, Word64) {
    unsafe { (Word64 { i : lhs.i / rhs.i }, Word64 { i : lhs.i % rhs.i }) }
  }

  #[test]
  fn expr_eval() {
    let A = box Var("a".to_string());
    let B = box Var("b".to_string());
    let X = box Var("x".to_string());
    let Y = box Var("y".to_string());

    let EXPR0 = box Lam("y".into(), X.clone());
    let EXPR1 = box Lam("x".into(), EXPR0.clone()); // \x. \y. x

    let EXPR2 = box App(EXPR1.clone(), A.clone()); // (\x. \y. x) a
    let EXPR3 = box App(EXPR1.clone(), B.clone()); // (\x. \y. x) b
    let EXPR4 = box App(EXPR2.clone(), B.clone()); // (\x. \y. x) a b

    // let IPLUS = box P(Fn21(&(iplus as fn(_, _) -> _)));
    // let IZERO : Box<Expr> = box PValue { i : 0 }.into();
    // let IONE : Box<Expr> = box PValue { i : 1 }.into();
    // let ITWO : Box<Expr> = box PValue { i : 2 }.into();
    // let IDIVMOD : Box<Expr> = box P(Fn22(&(idivmod as fn(_, _) -> (_, _))));
    // let IN15 : Box<Expr> = box PValue { i : 15 }.into();
    // let IN31 : Box<Expr> = box PValue { i : 31 }.into();

    // let IONE_PLUS_ONE = box App(IPLUS.clone(), box Cons(IONE.clone(),
    // IONE.clone())); let IONE_PLUS_ZERO = box App(IPLUS.clone(), box
    // Cons(IONE.clone(), IZERO.clone()));

    // let IN31_DIVMOD_TWO = box App(IDIVMOD.clone(), box Cons(IN31.clone(),
    // ITWO.clone())); let PRGM_IONE_IN15 = box
    // Program(vec![Stmt::Push(*IONE.clone()), Stmt::Push(*IN15.clone())]);

    assert_eq!(EXPR2.clone().subst(&"a".to_string(), &B).unwrap(), *EXPR3);
    assert_eq!(EXPR2.clone().subst(&"x".to_string(), &Y).unwrap(), *EXPR2);

    assert_eq!(EXPR4.clone().red().unwrap().red().unwrap(), *A);

    assert_eq!(EXPR4.simpl(), *A);
    // assert_eq!(IONE_PLUS_ONE.simpl(), *ITWO);
    // assert_eq!(IONE_PLUS_ZERO.simpl(), *IONE);
    // assert_eq!(IN31_DIVMOD_TWO.simpl(), *PRGM_IONE_IN15)
  }

  #[test]
  fn arith_constructions() {
    let A = box Var("a".to_string());
    let B = box Var("b".to_string());
    let X = box Var("x".to_string());
    let Y = box Var("y".to_string());

    let EXPR0 = box Cons(A.clone(), B.clone());
    let EXPR1 = box Inl(EXPR0.clone());
    let EXPR2 = box Inr(EXPR0.clone());
    let EXPR3 = box Cons(X.clone(), EXPR0.clone());
  }
}
