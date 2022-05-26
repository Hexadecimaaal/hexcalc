#[cfg(test)]
#[allow(non_snake_case)]
mod expr {


  use crate::expr::*;
  use crate::state::{Stack, State, Stmt};

  use alloc::string::ToString;
  use alloc::vec;
  use Expr::*;
  // use Primitive::*;

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
  fn algebra_constructions() {
    let A = box Var("a".to_string());
    let B = box Var("b".to_string());
    let X = box Var("x".to_string());
    let Y = box Var("y".to_string());

    let EXPR0 = box Cons(A.clone(), B.clone());
    let EXPR1 = box Inl(EXPR0.clone());
    let EXPR2 = box Inr(EXPR0.clone());
    let EXPR3 = box Cons(X.clone(), EXPR0.clone());
  }

  #[test]
  fn intrinsics_arith() {
    let mut state = State(Stack(vec![]), crate::intrinsics::generate_intrinsics());

    macro_rules! at {
      ($e:expr) => {
        state.0.at($e).unwrap()
      };
    }

    macro_rules! apply {
      ($e:expr) => {
        state.eval(state.1.get(&$e.to_string()).unwrap()).unwrap();
      };
    }

    apply!("ZERO");
    assert_eq!(at!(1), 0u64.into());

    apply!("ONE");
    assert_eq!(at!(2), 0u64.into());
    assert_eq!(at!(1), 1u64.into());

    apply!("ONE");
    apply!("PLUS");
    assert_eq!(at!(1), 2u64.into());

    apply!("SUB");
    assert_eq!(at!(1), (-2i64).into());

    apply!("NOT");
    assert_eq!(at!(1), 1u64.into());

    state.eval(Stmt::Push(4u64.into())).unwrap();
    apply!("SHL");
    assert_eq!(at!(1), 16u64.into());
  }
}
