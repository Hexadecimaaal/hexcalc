use nom::{
  IResult,
  character::{
    complete::{ char, digit0, hex_digit1 },
    is_hex_digit
  },
  number::complete::hex_u32, sequence::pair, bytes::complete::take_while, Parser
};

use crate::expr::Word64;

fn parse_u64(input : &str) -> IResult<&str, u64> {
  let (rest, p) = hex_digit1(input)?;
  Ok((rest, u64::from_str_radix(p, 16).unwrap()))
}

fn parse_i64(input : &str) -> IResult<&str, i64> {
  todo!()
}

fn parse_word(input : &str) -> IResult<&str, Word64> {
  todo!()
}
