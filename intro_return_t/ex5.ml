open Core.Std

type person = { name : (string * string)
	      ; age  : Int.t
	      ; zip  : string
	      }

(* A little helper function *)
let int_of_string s =
  try
    Some (Int.of_string s)
  with
    | Failure _ -> None

let parse_name name =
  Result.of_option
    (String.lsplit2 ~on:' ' name)
    ~error:(`Bad_name name)

let parse_age age =
  Result.of_option
    (int_of_string age)
    ~error:(`Bad_age age)

let parse_zip zip =
  match int_of_string zip with
  | Some _ when String.length zip = 5 -> Ok zip
  | _ -> Error (`Bad_zip zip)

let parse_person s =
  match String.split ~on:'\t' s with
    | [name; age; zip] ->
      let open Result.Monad_infix in
      parse_name name >>= fun name ->
      parse_age  age  >>= fun age  ->
      parse_zip  zip  >>= fun zip  ->
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
