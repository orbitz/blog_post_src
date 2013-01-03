open Core.Std

type person = { name : (string * string)
	      ; age  : Int.t
	      ; zip  : string
	      }

type parse_name_ret =
  | Name of (string * string)
  | Bad_name_input of string

type parse_age_ret =
  | Age of Int.t
  | Bad_age_input of string

type parse_zip_ret =
  | Zip of string
  | Bad_zip_input of string

type parse_info_ret =
  | Person of person
  | Bad_line of string
  | Bad_name of string
  | Bad_age  of string
  | Bad_zip  of string

type int_of_string =
  | Integer of Int.t
  | Bad_integer of string

(* A little helper function *)
let int_of_string s =
  try
    Integer (Int.of_string s)
  with
    | Failure _ ->
      Bad_integer s

let parse_name name =
  match String.lsplit2 ~on:' ' name with
    | Some (first_name, last_name) ->
      Name (first_name, last_name)
    | None ->
      Bad_name_input name

let parse_age age =
  match int_of_string age with
    | Integer v ->
      Age v
    | Bad_integer _ ->
      Bad_age_input age

let parse_zip zip =
    match (int_of_string zip, String.length zip = 5) with
      | (Bad_integer _, _) ->
	Bad_zip_input zip
      | (_, false) ->
	Bad_zip_input zip
      | (Integer _, true) ->
	Zip zip

let parse_person s =
  match String.split ~on:'\t' s with
    | [name; age; zip] -> begin
      match (parse_name name,
	     parse_age age,
	     parse_zip zip) with
	| (Name name, Age age, Zip zip) ->
	  Person { name; age; zip }
	| (Bad_name_input s, _, _) ->
	  Bad_name s
	| (_, Bad_age_input s, _) ->
	  Bad_age s
	| (_, _, Bad_zip_input s) ->
	  Bad_zip s
    end
    | _ ->
      Bad_line s

let () =
  (* Pretend input came from user *)
  let input = "Joe Mama\t25\t11425" in
  match parse_person input with
    | Person person ->
      printf "Name: %s %s\nAge: %d\nZip: %s\n"
	(fst person.name)
	(snd person.name)
	person.age
	person.zip
    | Bad_line l ->
      printf "Bad line: '%s'\n" l
    | Bad_name name ->
      printf "Bad name: '%s'\n" name
    | Bad_age age ->
      printf "Bad age: '%s'\n" age
    | Bad_zip zip ->
      printf "Bad zip: '%s'\n" zip
