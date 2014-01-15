open Unix
open Lwt
open Str

(* gestion d'erreur spéciale au module Unix *)
let print_error_unix e = match e with 
	| Unix.Unix_error (errcode,_,_) -> print_string (Unix.error_message errcode);
					   print_newline();
	| _ -> print_string (Printexc.to_string e)

(* Simplification énorme de lecture lwt sur descripteur de fichier *)
let lwt_blockread socket lgth =
	let s = String.create 8192 in
	Lwt_unix.wait_read socket >>= fun () ->
	Lwt_unix.read socket s 0 (String.length s) >>= fun len ->
	try begin
		let s = String.sub s 0 (if len < lgth then len else lgth) in
		return (s,(if len < lgth then len else lgth))
	end with e -> return (s,len)

(* Idem en écriture *)
let lwt_blockwrite socket s =
	Lwt_unix.wait_write socket >>= fun () ->
	Lwt_unix.write socket s 0 (String.length s)

(* Ferme une connection + un message donnant la raison *)
let lwt_closeconnection socket message =
	lwt_blockwrite socket message >>= fun len ->
	return (Lwt_unix.shutdown socket Unix.SHUTDOWN_ALL)

(* Formatte un string trop long ou contenant des caractères inappropriés. (comme telnet aime si bien le faire ...) *)
let format_string str len =
	try begin 
		let substr = (String.sub str 0 len) in 
		let _ = Str.search_forward (Str.regexp "[A-Za-z0-9_-]+") substr 0  in
		Str.matched_string substr
	end with e -> str

let format_string_cmd str len =
	try begin 
		let substr = (String.sub str 0 len) in 
		let _ = Str.search_forward (Str.regexp "[/A-Za-z0-9_ -]+") substr 0  in
		Str.matched_string substr
	end with e -> str

let format_string_number str len =
	try begin 
		let substr = (String.sub str 0 len) in 
		let _ = Str.search_forward (Str.regexp "[0-9]+") substr 0  in
		Str.matched_string substr
	end with e -> str

let format_string_strict str len =
	try begin 
		let substr = (String.sub str 0 len) in 
		let _ = Str.search_forward (Str.regexp "[A-Za-zàéùêâôîû]+") substr 0  in
		Str.matched_string substr
	end with e -> str

(* Vérifie si les chaines de caractères correspondent à des 'schémas' regex établis. *)
let check_string_pass pass =
	Str.string_match (regexp "[A-Za-z0-9]+") pass 0

let check_string_pseudo pseudo =
	Str.string_match (regexp "[A-Za-z0-9_-]+") pseudo 0

let check_string_mot mot =
	Str.string_match (regexp "[A-Za-zàéùêâôîû]+") mot 0

let check_string_nombre nb =
	Str.string_match (regexp "[0-9]+") nb 0

(* Fonctions de recherches dans des chaines de caractères *)
let parse_string pseudo mdp str = 
	try begin 
		let _ = Str.search_forward (Str.regexp ("^"^pseudo^" "^mdp^" "^"[0-9]+$")) str 0 
		in true
	end with e -> false

let parse_string_pseudo pseudo str = 
	try begin 
		let _ = Str.search_forward (Str.regexp ("^"^pseudo^" "^"[0-9a-zA-Z]+"^" "^"[0-9]+$")) str 0 
		in true
	end with e -> false

(* Conversions pratiques en string pour envoi par socket ... *)
let string_welcome pseudo score =
	let s = "\n-- Welcome to the pendu server, "^pseudo^" !\n--\n-- You have a capital of "^(string_of_int score)^" points.\n\n" in
	s
	
let score_to_string score = 
	if score < 10 then
		("00"^(string_of_int score))
	else if score < 100 then 
		("0"^(string_of_int score))
	else string_of_int score

let format_string_pendu mot lettres =
	let rec loop1 mot newmot = match mot with
		| "" -> newmot
		| s -> let fst = String.sub s 0 1 in
		       let tail = String.sub s 1 ((String.length s) - 1) in
		       loop1 tail (newmot^(loop lettres fst))
	and loop l lettre = match l with
		| [] -> "_"
		| a::b -> if a = lettre then a else loop b lettre
	in if lettres = [] then begin
			let newmot = String.create (String.length mot) in
			String.fill newmot 0 (String.length newmot) '_';
			newmot
		end
	else
		loop1 mot ""

let format_string_rates rates = 
	let rec loop l accu = match l with
		| [] -> accu
		| a::b -> loop b (accu^a)
	in loop rates ""