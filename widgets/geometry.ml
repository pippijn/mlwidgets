open Prelude


module Symbolic = struct
  open Sexplib.Conv

  type id =
    | Self
    | Parent
    | Name of string
    with sexp

  type expr =
    | Free                (** Unconstrained value.                       *)
    | Int   of int        (** Immediate integer value.                   *)
    | Float of float      (** Floating point value. Used in expressions. *)

    | Width  of id        (** Width of another widget.                   *)
    | Height of id        (** Height of another widget.                  *)
    | Left   of id        (** Left column of another widget.             *)
    | Top    of id        (** Upper row of another widget.               *)
    | Right  of id        (** Right column of another widget.            *)
    | Bottom of id        (** Bottom row of another widget.              *)

    | Add of expr * expr  (** Addition of two expressions.               *)
    | Sub of expr * expr  (** Subtraction of two expressions.            *)
    | Div of expr * expr  (** Division of two expressions.               *)
    | Mul of expr * expr  (** Multiplication of two expressions.         *)
    with sexp


  type size = {
    width  : expr;
    height : expr;
  } with sexp

  type position = {
    left   : expr;
    top    : expr;
    right  : expr;
    bottom : expr;
  } with sexp

  type geometry = {
    size : size;
    position : position;
  } with sexp


  let string_of_geometry =
    sexp_of_geometry
    |- Sexplib.Sexp.to_string_hum


  (** Default geometry completely fills its parent. *)
  let default = {
    position = {
      left   = Int 0;
      top    = Int 0;
      right  = Width  Parent;
      bottom = Height Parent;
    };
    size = {
      width  = Free;
      height = Free;
    };
  }


  (** A completely unconstrained geometry. *)
  let free = {
    position = {
      left   = Free;
      top    = Free;
      right  = Free;
      bottom = Free;
    };
    size = {
      width  = Free;
      height = Free;
    };
  }
end


module Concrete = struct
  open Sexplib.Conv

  type size = {
    width  : int;
    height : int;
  } with sexp

  type position = {
    x : int;
    y : int;
  } with sexp

  type geometry = {
    size : size;
    position : position;
  } with sexp


  let string_of_size =
    sexp_of_size
    |- Sexplib.Sexp.to_string_hum


  let string_of_position =
    sexp_of_position
    |- Sexplib.Sexp.to_string_hum


  let string_of_geometry =
    sexp_of_geometry
    |- Sexplib.Sexp.to_string_hum
end


module Map = String_map.Make (struct type t = Concrete.geometry end)

include Symbolic
