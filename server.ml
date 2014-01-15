open Lwt
open Utils
open Unix
open Str

exception DEFI_INTROUVABLE of string

(* Quelques constantes *)
let local_addr num = Unix.ADDR_INET (Unix.inet_addr_any , num)
let max_rates = 5

let dict_server = "dict.org"
let dict_port = 2628 

(* == DEBUT dico == *)

(* Connection au dictionnaire en ligne *)
let connect_dico () = 
	let dict_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	let dict_addr = List.hd (Unix.getaddrinfo dict_server (string_of_int dict_port) []) in
	Lwt_unix.connect dict_socket (dict_addr.ai_addr) >>= fun () ->
	return dict_socket

(* Déconnection *)
let disconnect_dico socket =
	lwt_blockwrite socket "QUIT"

(* Demande au dictionnaire si le mot existe *)
let ask_dico word =
	connect_dico () >>= fun socket ->
	lwt_blockread socket 1024 >>= fun (res,len) ->
	let s = "MATCH all exact "^word^" \n" in
	lwt_blockwrite socket s >>= fun len ->
	lwt_blockread socket 1024 >>= fun (res,len) ->
	let _ = Str.search_forward (Str.regexp "^[0-9]+") res 0  in
	let res_code = Str.matched_string res in
	if res_code = "152" then begin
		lwt_blockread socket 1024 >>= fun (res,len) ->
		let splitted_result = Str.split (regexp "$") res in
		disconnect_dico socket >>= fun len ->
		let dico_line = (List.hd splitted_result) in
		let dico = List.hd (Str.split (regexp " +") dico_line) in
		return dico
	end
	else begin
		disconnect_dico socket >>= fun len ->
		return ""
	end

(* == FIN dico == *)

(* == DEBUT script google == *)

(* Effectue le script google fourni et classe le résultat en catégories *)
let check_script mot = 
	Lwt_process.pread (Lwt_process.shell ("./script_google "^mot)) >>= fun s ->
	let s = format_string_number s (String.length s) in
	try begin
	let occur = int_of_string s in
	if occur < 1000000 then return "rare" 
	else if occur < 50000000 then return "common"
	else return "well known"
	end with e -> return "?"


(* == FIN script google == *)

(* == DEBUT défi == *)

(* Type défi contenant tous les champs nécessaires *)
type defi = { 
	      auteur : string;
	      enjeu : int;
	      mot : string;
	      taille : int;
	      frequence : string;
	      mutable won : int;
	      mutable lost : int;
	      dictionnaire : string;
	    }

(* Pile de défis actifs *)
let defi_pile = ref []

(* Fonctions pratiques pour affichage *)
let defi_to_string d num = 
	("["^(string_of_int num)^"]| "^d.auteur^" | "^(string_of_int d.enjeu)^" pts | "^(string_of_int d.taille)^" | "^d.frequence^" | "^(string_of_int d.won)^"/"^(string_of_int d.lost)^" | "^d.dictionnaire^" |")

let defis_to_string () = 
	let rec loop l accu num = match l with
		| [] -> accu
		| a::b -> loop b (accu^(defi_to_string a num)^"\n") (num+1)
	in let s = "\n[n°]| Author | Bet | Length | Frequency | W/L | Dictionary |\n" 
	in let s = s^"     ---------------------------------------------------------- "
	in s^(loop !defi_pile "\n" 0)^"\n"


(*Opérations élémentaires des piles *)

(* Vérifie si un défi passé en argument existe déjà dans la pile ou non *)
let exists_defi d = 
	let rec loop l = match l with
		| [] -> false
		| a::b -> if a.auteur = d.auteur && a.mot = d.mot then true else loop b
	in loop !defi_pile

(* Ajout *)
let add_defi (d:defi) =
	defi_pile := (d::!defi_pile)

(* Retrait *)
let remove_defi d =
	let rec loop l accu = match l with
		| [] -> accu
		| a::b -> if a.auteur = d.auteur && a.mot = d.mot then (loop b accu) else (loop b (a::accu))
	in defi_pile := List.rev (loop !defi_pile [])

(* Ajout de gain / perte (won / lost) [mutables] *)
let add_gp gp d =
	let rec loop l = match l with
		| [] -> ()
		| a::b -> if a.auteur = d.auteur && a.mot = d.mot then begin
				if gp then begin a.won <- a.won + 1 end
				else begin a.lost <- a.lost + 1 end
			  end else loop b
	in loop !defi_pile

(* Nombre de défis actifs *)
let nbdefis () =
	let rec loop l accu = match l with
		| [] -> accu
		| a::b -> loop b (accu+1)
	in loop !defi_pile 0

(* Extrait un défi de par sa position dans la pile *)
let get_defi nb = 
	let rec loop l accu = match l with
		| [] -> raise (DEFI_INTROUVABLE " - Fonction get_defi.\n")
		| a::b -> if accu = nb then a else loop b (accu+1)
	in loop !defi_pile 0

(* Création de défi *)
let rec defi_creation socket pseudo =
	let s = "\n-- Your secret word : " in
	lwt_blockwrite socket s >>= fun len ->
	lwt_blockread socket 35 >>= fun (word,len) ->
	let word = format_string_strict word len in
	if not (check_string_mot word) then begin
		let s = "\n-- Invalid word format.\n\n" in
		lwt_blockwrite socket s >>= fun len ->
		return () 
	end else return () >>= fun () ->
	let s = "\n-- Verification ... " in
	lwt_blockwrite socket s >>= fun len ->
	ask_dico word >>= fun dico_name ->
	if dico_name = "" then begin
		let s = "\n-- Unable to find a dictionary containing the word.\n\n" in
		lwt_blockwrite socket s >>= fun len ->
		return () 
	end else return ("Word found in dictionary : "^dico_name^"\n") >>= fun s ->
	lwt_blockwrite socket s >>= fun len -> 
	check_script word >>= fun occur ->
	let s = "\n-- This word is rather : "^occur^"\n" in
	lwt_blockwrite socket s >>= fun len ->
	let s = "\n-- Reward : " in
	lwt_blockwrite socket s >>= fun len ->
	lwt_blockread socket 4 >>= fun (nb,len) ->
	let nb = format_string_number nb len in
	if not (check_string_nombre nb) then begin
		let s = "\n-- Invalid word format.\n\n" in
		lwt_blockwrite socket s >>= fun len ->
		return () 
	end else return () >>= fun () -> 
	get_score pseudo >>= fun score ->
	if (int_of_string nb) > score then begin
		let s = "\n-- You cannot bet more than your own score.\n\n" in
		lwt_blockwrite socket s >>= fun len ->
		return () 
	end else return () >>= fun () -> 
	let d = {auteur = pseudo; enjeu = (int_of_string nb); mot = word; taille = (String.length word); frequence = occur; won = 0; lost = 0; dictionnaire = dico_name } in
	if exists_defi d then begin
		let s = "\n-- The challenge already exists.\n\n" in
		lwt_blockwrite socket s >>= fun len ->
		return () 
	end else return () >>= fun () ->
	add_defi d;
	let s = "\n-- Challenge added !\n\n" in
	lwt_blockwrite socket s >>= fun len ->
	return ()

(* == FIN défi == *)

(* == DEBUT jeu == *)

(* Boucle de jeu lancé *)
and loop_jeu socket d rates reussis pseudo =
	let mot = format_string_pendu d.mot reussis in
	if not (String.contains mot '_') then
		begin
			let s = ("-- Congratulations !\n-- You found the word : "^d.mot^"\n-- You win : "^(string_of_int d.enjeu)^" points.\n\n") in
			lwt_blockwrite socket s >>= fun len ->
			add_gp true d;
			add_score d.auteur (-d.enjeu) >>= fun () ->
			add_score pseudo d.enjeu >>= fun () ->
			return ()
		end
	else begin
		let s = ("-- Word : "^mot^"\n") in
		let s = s^"-- Misses : ["^(format_string_rates rates)^"]\n\n-- Your choice : " in
		lwt_blockwrite socket s >>= fun len ->
		lwt_blockread socket 1 >>= fun (letter,len) ->
		if String.contains d.mot letter.[0] then 
			loop_jeu socket d rates (letter::reussis) pseudo
		else begin 
			if List.length rates > max_rates then begin
				let s = ("-- You lost !\n-- The word was : "^d.mot^"\n-- You lose : "^(string_of_int d.enjeu)^" points.\n\n") in
				lwt_blockwrite socket s >>= fun len ->
				add_gp false d;
				add_score d.auteur d.enjeu >>= fun () ->
				add_score pseudo (-d.enjeu) >>= fun () ->
				return ()
			end else 
				loop_jeu socket d (letter::rates) reussis pseudo
		end
	end

(* Lancement d'un défi *)
and main_jeu socket nb pseudo = 
	let s = "\n-- Challenge accepted!\n" in
	lwt_blockwrite socket s >>= fun len ->
	let d = get_defi (int_of_string nb) in
	loop_jeu socket d [] [] pseudo

(* Choix d'un défi *)
and init_jeu socket pseudo =
	let s = defis_to_string () in
	lwt_blockwrite socket s >>= fun len ->
	let s = "-- Pick a challenge : " in
	lwt_blockwrite socket s >>= fun len ->
	lwt_blockread socket 4 >>= fun (nb,len) ->
	let nb = format_string_number nb len in
	if not (check_string_nombre nb) then begin
		let s = "\n-- Invalid word format.\n\n" in
		lwt_blockwrite socket s >>= fun len ->
		return () 
	end else return () >>= fun () -> 
	if (int_of_string nb) >= nbdefis () then begin
		let s = "\n-- Challenge number invalid.\n\n" in
		lwt_blockwrite socket s >>= fun len ->
		return () 
	end else
		main_jeu socket nb pseudo

(* == FIN jeu == *)

(* Boucle de lecture de commandes principale *)
and loop_message output pseudo = 
	let s = pseudo^"@pendu > " in
	lwt_blockwrite output s >>= fun len ->
	let s = String.create 100 in
	Lwt_unix.wait_read output >>= fun () ->
	Lwt_unix.read output s 0 100 >>= fun len ->
	let s = format_string_cmd s len in
	if len = 0 then begin lwt_closeconnection output "\n-- Thank you for playing !\n-- See you next time !\n\n" end
	else begin
		(* Log *)
		print_string (pseudo^" |> "^s);
		print_newline ();
		parse_cmd pseudo s output 
	end

(* Parsing de commandes ... *)
and parse_cmd pseudo str socket = 
	match (List.hd (Str.split (Str.regexp " +") str)) with 
	| "/quit" -> lwt_closeconnection socket "\n-- Thank you for playing !\n-- See you next time !\n\n"
	| "/scores" -> get_allscores () >>= fun scores ->
		      lwt_blockwrite socket scores  >>= fun len ->
		      loop_message socket pseudo
	| "/challenge" ->  defi_creation socket pseudo >>= fun () ->
		      loop_message socket pseudo
	| "/challengelist" -> let s = defis_to_string () in
			   lwt_blockwrite socket s >>= fun len ->
		           loop_message socket pseudo
	| "/play" -> init_jeu socket pseudo >>= fun () ->
		      loop_message socket pseudo
	| "/help" ->  let s = "\n|          Server pendu [HELP]          |\n" in
		      let s =   s^" ----------------------------------------\n" in
		      let s =   s^"| /quit          -> Disconnect and close  |\n" in
		      let s =   s^"| /scores        -> Show the scores       |\n" in
		      let s =   s^"| /challenge     -> Start a challenge     |\n" in
		      let s =   s^"| /challengelist -> Show challenges       |\n" in
		      let s =   s^"| /play          -> Accept a challenge    |\n\n" in
		      lwt_blockwrite socket s  >>= fun len ->
		      loop_message socket pseudo
	| _ -> lwt_blockwrite socket ("-- Unknown command : "^str^"\n-- Type /help for the description of the commands.\n")  >>= fun len ->
	       loop_message socket pseudo

(* == DEBUT IO == *)
(* /!\ 8192 caractères MAXIMUM dans le fichier id.txt *)

(* Vérifie dans le fichier id si le mot de passe correspond au pseudo *)
and check_auth pseudo mdp =
	let fd = Lwt_unix.of_unix_file_descr (Unix.openfile "id.txt" [Unix.O_RDONLY] 640) in
	lwt_blockread fd 8192 >>= fun (buffer,len) ->
	Lwt_unix.close fd;
	if parse_string pseudo mdp buffer then (return true)
	else (return false)
	
(* Vérifie dans le fichier id si le pseudo existe *)
and check_pseudo pseudo =
	let fd = Lwt_unix.of_unix_file_descr (Unix.openfile "id.txt" [Unix.O_RDONLY] 640) in
	lwt_blockread fd 8192 >>= fun (buffer,len) ->
	Lwt_unix.close fd;
	if parse_string_pseudo pseudo buffer then (return true)
	else (return false)

(* Ecris de nouvelles infos de compte dans le fichier id *)
and write_compte pseudo mdp =
	let fd = Lwt_unix.of_unix_file_descr (Unix.openfile "id.txt" (Unix.O_WRONLY::Unix.O_APPEND::[]) 640) in
	let buffer = pseudo^" "^mdp^" 010\n" in
	lwt_blockwrite fd buffer >>= fun len ->
	Lwt_unix.close fd;
	return ()

(* Donne le score d'un utilisateur *)
and get_score pseudo =
	let fd = Lwt_unix.of_unix_file_descr (Unix.openfile "id.txt" [Unix.O_RDONLY] 640) in
	lwt_blockread fd 8192 >>= fun (buffer,len) ->
	Lwt_unix.close fd;
	try begin 
		let _ = Str.search_forward (Str.regexp ("^"^pseudo^" "^"[0-9a-zA-Z]+"^" "^"[0-9]+$")) buffer 0 
		in let substr = Str.matched_string buffer in
		let _ = Str.search_forward (Str.regexp ("[0-9]+$")) substr 0 in
		let score_str = Str.matched_string substr in
		return (int_of_string score_str)
	end with e -> (return (-1))

(* Donne une chaine de caractère formattée contenant les scores de tous les joueurs enregistrés *)
and get_allscores () =
	let fd = Lwt_unix.of_unix_file_descr (Unix.openfile "id.txt" [Unix.O_RDONLY] 640) in
	lwt_blockread fd 8192 >>= fun (buffer,len) ->
	Lwt_unix.close fd;
	let all_lines = Str.split (regexp "$") buffer in
	let rec loop_lines liste accu = match liste with
		| [] -> (accu^"\n")
		| a::b ->  loop_lines b (accu^"| "^(
				try begin 
				let add = "" in
				let _ = Str.search_forward (Str.regexp ("^[a-zA-Z0-9-_]+")) a 0 in
				let add = (add^(Str.matched_string a)^" : ") in
				let _ = Str.search_forward (Str.regexp ("[0-9]+$")) a 0 in
				(add^(Str.matched_string a)^" pts\n")
				end with e -> begin "" end
				))
	in return (loop_lines all_lines "\n-------[Scores]------\n| \n")

(* Modifie un score *)
and add_score pseudo nb =
	let fd = Lwt_unix.of_unix_file_descr (Unix.openfile "id.txt" [Unix.O_RDWR] 640) in
	lwt_blockread fd 8192 >>= fun (buffer,len) ->
	try begin
		let scorepos = Str.search_forward (Str.regexp ("^"^pseudo^" "^"[0-9a-zA-Z]+"^" "^"[0-9]+$")) buffer 0 
		in let substr = Str.matched_string buffer in
		let scorepos = scorepos + (Str.search_forward (Str.regexp ("[0-9]+$")) substr 0) in
		let _ = Unix.lseek (Lwt_unix.unix_file_descr fd) scorepos Unix.SEEK_SET in
		get_score pseudo >>= fun last_score ->
		let newscore = (if (nb+last_score) < 0 then 0 else (nb+last_score)) in
		lwt_blockwrite fd (score_to_string newscore) >>= fun len ->
		Lwt_unix.close fd;
		return ()
	end with e -> return ()

(* == FIN IO == *)

(* == DEBUT authentification == *)

(* Demande d'infos à l'utilisateur *)
and ask_pseudo socket =
	let s = "\n-- Enter username.\n" in
	lwt_blockwrite socket s >>= fun len ->
	lwt_blockread socket 30 >>= fun (pseudo,len) ->
	(* for testing ...  
	   print_string "length pseudo :";
	   print_int len; print_newline (); 
	   print_string ("pseudo : "^pseudo);
	   print_newline (); *)
	let pseudo = format_string pseudo len in
	return pseudo
and ask_pwd socket =
	let s = "\n-- Enter password.\n" in
	lwt_blockwrite socket s >>= fun len ->
	lwt_blockread socket 30 >>= fun (pwd,len) ->
	(*for testing ... 
	   print_string "length mdp :";
	   print_int len; print_newline (); 
	   print_string ("mdp : "^pwd);
	   print_newline (); *)
	let pwd = format_string pwd len in
	return pwd

(* Premier contact ... *)
and shake_hands socket =
	print_string ("-- Connection received."); print_newline ();
	ask_pseudo socket >>= fun pseudo ->
	if check_string_pseudo pseudo then 
		check_pseudo pseudo >>= fun pseudo_exists ->
		if pseudo_exists then
			finish_auth socket pseudo
		else create_compte socket pseudo
	else lwt_closeconnection socket "\n-- Invalid username format.\n"

(* Le pseudo existe, on finit l'authentification *)
and finish_auth socket pseudo =
	ask_pwd socket >>= fun pwd ->
	(* for testing ...
	   print_string ("pseudo : "^pseudo^" | pwd : "^pwd);
	   print_newline (); *)
	check_auth pseudo pwd >>= fun b ->
	if b then begin
		get_score pseudo >>= fun i -> 
		lwt_blockwrite socket ("\n-- Account found.\n"^(string_welcome pseudo i)) >>= fun len ->
		print_string ("-- "^pseudo^" > Login OK."); print_newline ();
		loop_message socket pseudo
	end else begin 
		lwt_closeconnection socket "\n-- Account not found or incorrect password.\n-- See you !\n\n"
	end

(* Le pseudo n'existe pas, on crée le compte *)
and create_compte socket pseudo = 
	let s = "\n-- Account not found. Do you want to create an account ? (\"y\" means yes)\n" in
	lwt_blockwrite socket s >>= fun len ->
	lwt_blockread socket 8192 >>= fun (answer,len) ->
	let answer = format_string answer len in
	if answer = "y" then begin
		ask_pwd socket >>= fun mdp ->
		write_compte pseudo mdp >>= fun () ->
		let s = ("\n-- Account created.\n"^(string_welcome pseudo 10)) in
		lwt_blockwrite socket s >>= fun len ->
		print_string ("-- "^pseudo^" > New account."); print_newline ();
		loop_message socket pseudo
	end else 
		lwt_closeconnection socket "\n-- See you !\n\n"

(* == FIN authentification == *)


(* Connection d'un utilisateur et thread dédié ... *)
and exec_accept listening output =
	ignore(Lwt.catch (fun () -> shake_hands output) 
			 (fun e -> print_error_unix e; Lwt.fail e));
	(Lwt.catch (fun () -> Lwt_unix.accept listening) 
		   (fun e -> print_error_unix e; Lwt.fail e)) >>= fun (socket, socknb) ->
	exec_accept listening socket

(* Fonction main *)
let _ =
	let listening_port = 
		if Array.length Sys.argv >= 2 then (int_of_string Sys.argv.(1))
		else 15555
	in
	Lwt_unix.run begin
		let listening_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
		Lwt_unix.bind listening_socket (local_addr listening_port);
		Lwt_unix.listen listening_socket 10;
		print_string ("-- Server online. Waiting for connections."); print_newline ();
		Lwt_unix.accept listening_socket >>= fun (socket, socknb) ->
		exec_accept listening_socket socket
	end
