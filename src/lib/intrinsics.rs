use core::{num::Wrapping, ops::Not};

use alloc::{collections::BTreeMap, string::String, vec};

use crate::{
  errors::Error,
  expr::{Expr, Word64},
  state::{Context, Stack, State, Stmt},
};

fn generate_intrinsics() -> BTreeMap<String, Stmt> {
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
        Command(&(sub as fn(&mut _) -> _)),
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
        Command(&(sub as fn(&mut _) -> _)),
      )
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

  intr
}
