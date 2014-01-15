open Lwt
open Utils

(* Boucle de test inutile *)
let rec loop_message output s = 
	Lwt_unix.wait_write output >>= fun () ->
	Lwt_unix.write output s 0 (String.length s) >>= fun len ->
	loop_read output
	
(* Boucle d'écriture et d'écoute clavier *)
and loop_write output =
	Lwt_io.read_line Lwt_io.stdin >>= fun s ->
	Lwt_unix.write output s 0 (String.length s) >>= fun len ->
	loop_write output

(* Boucle de lecture du traffic réseau entrant et d'affichage *)
and loop_read output =
	let s = String.create 4096 in
	Lwt_unix.wait_read output >>= fun () ->
	Lwt_unix.read output s 0 4096 >>= fun len ->
	if len = 0 then return () 
	else begin
		print_string s;
		loop_read output 
	end

(* Connection  selon le port spécifié à un serveur TCP distant *)
let _ =
	let connect_port = 
		if Array.length Sys.argv >= 2 then (int_of_string Sys.argv.(1))
		else 15555
	in
	Lwt_unix.run begin
		let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
		Lwt.catch (fun () -> Lwt_unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_loopback,connect_port))) (fun e -> exit 1) >>= fun () ->
		ignore (Lwt.catch (fun () -> loop_write socket) 
			          (fun e -> print_error_unix e; Lwt.fail e));
		Lwt.catch (fun () -> loop_read socket) 
			  (fun e -> print_error_unix e; Lwt.fail e);
	end
