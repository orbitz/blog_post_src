open Core.Std

exception Int_of_string of string

exception Bad_line of string
exception Bad_name of string
exception Bad_age of string
exception Bad_zip of string

type person = { name : (string * string)
	      ; age  : Int.t
	      ; zip  : string
	      }

(* A little helper function *)
let int_of_string s =
  try
    Int.of_string s
  with
    | Failure _ ->
      raise (Int_of_string s)


let parse_name name =
  match String.lsplit2 ~on:' ' name with
    | Some (first_name, last_name) ->
      (first_name, last_name)
    | None ->
      raise (Bad_name name)

let parse_age age =
  try
    int_of_string age
  with
    | Int_of_string _ ->
      raise (Bad_age age)

let parse_zip zip =
  try
    ignore (int_of_string zip);
    if String.length zip = 5 then
      zip
    else
      raise (Bad_zip zip)
  with
    | Int_of_string _ ->
      raise (Bad_zip zip)

let parse_person s =
  match String.split ~on:'\t' s with
    | [name; age; zip] ->
      { name = parse_name name
      ; age  = parse_age age
      ; zip  = parse_zip zip
      }
    | _ ->
      raise (Bad_line s)

let () =
  (* Pretend input came from user *)
  let input = "Joe Mama\t25\t11425" in
  try
    let person = parse_person input in
    printf "Name: %s %s\nAge: %d\nZip: %s\n"
      (fst person.name)
      (snd person.name)
      person.age
      person.zip
  with
    | Bad_line l ->
      printf "Bad line: '%s'\n" l
    | Bad_name name ->
      printf "Bad name: '%s'\n" name
    | Bad_age age ->
      printf "Bad age: '%s'\n" age
    | Bad_zip zip ->
      printf "Bad zip: '%s'\n" zip
