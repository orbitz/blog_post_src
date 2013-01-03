open Core.Std

type person = { name : (string * string)
	      ; age  : Int.t
	      ; zip  : string
	      }

type parse_info_err =
  | Bad_line of string
  | Bad_name of string
  | Bad_age  of string
  | Bad_zip  of string

(* A little helper function *)
let int_of_string s =
  try
    Ok (Int.of_string s)
  with
    | Failure _ -> Error s

let parse_name name =
  match String.lsplit2 ~on:' ' name with
    | Some (first_name, last_name) ->
      Ok (first_name, last_name)
    | None ->
      Error name

let parse_age age =
  match int_of_string age with
    | Ok v ->
      Ok v
    | Error _ ->
      Error age

let parse_zip zip =
    match (int_of_string zip, String.length zip = 5) with
      | (Error _, _) ->
	Error zip
      | (_, false) ->
	Error zip
      | (Ok _, true) ->
	Ok zip

let parse_person s =
  match String.split ~on:'\t' s with
    | [name; age; zip] -> begin
      match (parse_name name,
	     parse_age age,
	     parse_zip zip) with
	| (Ok name, Ok age, Ok zip) ->
	  Ok { name; age; zip }
	| (Error s, _, _) ->
	  Error (Bad_name s)
	| (_, Error s, _) ->
	  Error (Bad_age s)
	| (_, _, Error s) ->
	  Error (Bad_zip s)
    end
    | _ ->
      Error (Bad_line s)

let () =
  (* Pretend input came from user *)
  let input = "Joe Mama\t25\t11425" in
  match parse_person input with
    | Ok person ->
      printf "Name: %s %s\nAge: %d\nZip: %s\n"
	(fst person.name)
	(snd person.name)
	person.age
	person.zip
    | Error (Bad_line l) ->
      printf "Bad line: '%s'\n" l
    | Error (Bad_name name) ->
      printf "Bad name: '%s'\n" name
    | Error (Bad_age age) ->
      printf "Bad age: '%s'\n" age
    | Error (Bad_zip zip) ->
      printf "Bad zip: '%s'\n" zip
