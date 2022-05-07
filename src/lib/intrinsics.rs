use core::{num::Wrapping, ops::Not};

use alloc::{collections::BTreeMap, string::String, vec};

use crate::{
  errors::Error,
  expr::{Expr, Word64},
  state::{State, Stmt, Variables},
};

use num_traits::real::Real;

pub fn generate_intrinsics() -> Variables {
  use Expr::*;
  use Stmt::*;
  let mut intr = BTreeMap::<String, Stmt>::new();

  intr.insert(
    "ONE".into(),
    Push(Expr::Word(Word64 { u : Wrapping(1u64) })),
  );
  intr.insert(
    "ZERO".into(),
    Push(Expr::Word(Word64 { u : Wrapping(0u64) })),
  );

  macro_rules! binary_word_op {
    ($fun:ident = $a:ident $b:ident -> $res:expr) => {
      fn $fun(state : &mut State) -> Result<(), Error> {
        let $b = state.0.pop()?;
        let $a = state.0.pop()?;
        match ($a, $b) {
          (Word($a), Word($b)) => {
            state.0.push(Word($res));
            Ok(())
          }
          ($a, $b) => panic!("{} called on ({:?}, {:?})", stringify!($fun), $a, $b),
        }
      }
      intr.insert(
        stringify!($fun).to_uppercase(),
        Command(&($fun as fn(&mut _) -> _)),
      )
    };
  }

  macro_rules! unary_word_op {
    ($fun:ident = $a:ident -> $res:expr) => {
      fn $fun(state : &mut State) -> Result<(), Error> {
        let $a = state.0.pop()?;
        match ($a) {
          Word($a) => {
            state.0.push(Word($res));
            Ok(())
          }
          $a => panic!("{} called on ({:?})", stringify!($fun), $a),
        }
      }
      intr.insert(
        stringify!($fun).to_uppercase(),
        Command(&($fun as fn(&mut _) -> _)),
      )
    };
  }

  macro_rules! state_stmt {
    ($fun:ident = $stmt:expr) => {
      intr.insert(
        stringify!($fun).to_uppercase(),
        Command(&($stmt as fn(&mut _) -> _)),
      );
    };
  }

  binary_word_op!(plus = a b -> Word64 { i : unsafe { a.i + b.i } });
  binary_word_op!(sub = a b -> Word64 { i : unsafe { a.i - b.i } });
  binary_word_op!(mul = a b -> Word64 { i : unsafe { a.i * b.i } });
  binary_word_op!(div = a b -> Word64 { i : unsafe { a.i / b.i } });
  binary_word_op!(rem = a b -> Word64 { i : unsafe { a.i % b.i } });
  binary_word_op!(pow = a b -> Word64 { i : unsafe { a.i.pow(b.i.0 as u32) } });
  binary_word_op!(and = a b -> Word64 { u : unsafe { a.u & b.u } });
  binary_word_op!(or = a b -> Word64 { u : unsafe { a.u | b.u } });
  binary_word_op!(xor = a b -> Word64 { u : unsafe { a.u ^ b.u } });
  binary_word_op!(shl = a b -> Word64 { u : unsafe { Wrapping(a.u.0 << b.u.0) } });
  binary_word_op!(shr = a b -> Word64 { u : unsafe { Wrapping(a.u.0 >> b.u.0) } });
  binary_word_op!(shra = a b -> Word64 { i : unsafe { Wrapping(a.i.0 >> b.u.0) } });
  binary_word_op!(rol = a b -> Word64 { u : unsafe { a.u.rotate_left(b.u.0 as u32) } });
  binary_word_op!(ror = a b -> Word64 { u : unsafe { a.u.rotate_right(b.u.0 as u32) } });

  unary_word_op!(not = a -> Word64 { u : unsafe { !a.u }});
  unary_word_op!(chs = a -> Word64 { i : unsafe { -a.i }});
  unary_word_op!(incr = a -> Word64 { u : unsafe { a.u + Wrapping(1u64) }});
  unary_word_op!(decr = a -> Word64 { u : unsafe { a.u - Wrapping(0u64) }});
  unary_word_op!(sign = a -> Word64 { i : unsafe { a.i.signum() }});
  unary_word_op!(rev = a -> Word64 { u : unsafe { a.u.reverse_bits() }});

  binary_word_op!(fplus = a b -> Word64 { f : unsafe { a.f + b.f } });
  binary_word_op!(fsub = a b -> Word64 { f : unsafe { a.f - b.f } });
  binary_word_op!(fmul = a b -> Word64 { f : unsafe { a.f * b.f } });
  binary_word_op!(fdiv = a b -> Word64 { f : unsafe { a.f / b.f } });
  binary_word_op!(frem = a b -> Word64 { f : unsafe { a.f % b.f } });
  binary_word_op!(fpow = a b -> Word64 { f : unsafe { a.f.powf(b.f) } });

  unary_word_op!(fchs = a -> Word64 { f : unsafe { -a.f }});
  unary_word_op!(finc = a -> Word64 { f : unsafe { a.f + 1.0 }});
  unary_word_op!(fdec = a -> Word64 { f : unsafe { a.f - 1.0 }});
  unary_word_op!(fsgn = a -> Word64 { f : unsafe { a.f.signum() }});
  unary_word_op!(sqrt = a -> Word64 { f : unsafe { a.f.sqrt() }});
  unary_word_op!(exp = a -> Word64 { f : unsafe { a.f.exp() }});
  unary_word_op!(ln = a -> Word64 { f : unsafe { a.f.ln() }});

  state_stmt!(drop = State::drop);
  state_stmt!(dup = State::dup);
  state_stmt!(swap = State::swap);
  state_stmt!(dropn = State::dropn);
  state_stmt!(roll = State::roll);
  state_stmt!(rolld = State::rolld);
  state_stmt!(rot = State::rot);
  state_stmt!(pick = State::pick);
  state_stmt!(over = State::over);

  state_stmt!(sto = State::sto);
  state_stmt!(rcl = State::rcl);
  state_stmt!(exch = State::exch);
  state_stmt!(purge = State::purge);

  Variables(intr)
}
