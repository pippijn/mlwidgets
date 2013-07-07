open CorePervasives
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
