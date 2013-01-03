open Core.Std

type person = { name : (string * string)
	      ; age  : Int.t
	      ; zip  : string
	      }

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
      Error (`Bad_name name)

let parse_age age =
  match int_of_string age with
    | Ok v ->
      Ok v
    | Error _ ->
      Error (`Bad_age age)

let parse_zip zip =
    match (int_of_string zip, String.length zip = 5) with
      | (Error _, _) ->
	Error (`Bad_zip zip)
      | (_, false) ->
	Error (`Bad_zip zip)
      | (Ok _, true) ->
	Ok zip

let parse_person s =
  match String.split ~on:'\t' s with
    | [name; age; zip] ->
      let open Result.Monad_infix in
      parse_name name >>= fun name ->
      parse_age  age  >>= fun age ->
      parse_zip  zip  >>= fun zip ->
      Ok { name; age; zip }
    | _ ->
      Error (`Bad_line s)

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
    | Error (`Bad_line l) ->
      printf "Bad line: '%s'\n" l
    | Error (`Bad_name name) ->
      printf "Bad name: '%s'\n" name
    | Error (`Bad_age age) ->
      printf "Bad age: '%s'\n" age
    | Error (`Bad_zip zip) ->
      printf "Bad zip: '%s'\n" zip
