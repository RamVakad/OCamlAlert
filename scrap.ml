(* ocamlfind c -w A -linkpkg -package lwt,lwt.unix,lwt.syntax -syntax camlp4o,lwt.syntax myecho.ml -o myecho *)
(* This code refers to https://github.com/avsm/ocaml-cohttpserver/blob/master/server/http_tcp_server.ml *)
open Lwt
open Lwt_io

type client = {
    oChan : Lwt_io.output Lwt_io.channel;
    iChan : Lwt_io.input Lwt_io.channel
}

let rec clientHandler client () =
    Lwt_io.read_line_opt client.iChan >>= (
        fun msg ->
            match msg with
            | Some msg -> (
                    if (msg = "KEEP-ALIVE") then (
                        Lwt_io.write_line client.oChan "KEEP-ALIVE" >>= clientHandler client
                    ) else (
                        Logs_lwt.info (fun m -> m "Invalid Message Received") >>= return
                    )
            )
            | None -> Logs_lwt.info (fun m -> m "One Alert Client Disconnected") >>= return
    )


(* -------------------------------------------------------------------------------------------------------- *)
                                        (* Admin Handling Code *)

(* The second message during the admin handshake needs to be equal to this for authentication. *)
let admin_SECRET = "SuperSecretPassword" 

(* Function to push an alert to everyone in the list. *)
let rec pushAlert alert clientList =
    match clientList with
    | client :: tl ->  
        print_endline("Pushing Alert to One Client");
        Lwt.catch ( fun () -> 
            Lwt_io.write_line client.oChan alert;
            Lwt.return_nil
        )
        (   function
            | Not_found -> print_endline (": Host not found"); Lwt.return_nil
            | e -> print_endline (": Host not found"); Lwt.return_nil
        );
        print_endline("Finished");
        pushAlert alert tl
    | [] -> ()

let rec adminHandler client clientList () =
    Lwt_io.read_line_opt client.iChan >>= (
        fun msg ->
            match msg with
            | Some msg -> pushAlert msg clientList; Logs_lwt.info (fun m -> m "New Alert Pushed") >>= adminHandler client clientList
            | None -> Logs_lwt.info (fun m -> m "Admin Client Disconnected") >>= return
    )

(* -------------------------------------------------------------------------------------------------------- *)

(*  Function: handshakeHandler()
    Accepts Parameters - { conn : The TCP/IP Connection Object }
    Behavior: Accesses the "file descriptor" of the connection and then extracts the input
                and output streams of the connection.
                The first message is read from the input stream and the handshake is initiated.
                If the first message is "iAmClient", the client is automatically added to the clientList and
                is passed onto the clientHandler.
                If the first message is "iAmAdmin", the second message is read and if it matches the Administator
                Secret Key, the connection is passed onto the adminHandler.
                If the wrong key is provided or an invalid message is received, the connection is dropped.    *)

let rec handshakeHandler client clientList () =
    print_endline "what";
    Lwt_io.read_line_opt client.iChan >>= (
        fun firstMessage ->
            match firstMessage with
            | Some firstMessage ->
                if (firstMessage = "iAmClient") then (
                    Lwt.on_failure (clientHandler client ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
                    Logs_lwt.info (fun m -> m "Client Handshake Successful") >>= return
                ) else if (firstMessage = "iAmAdmin") then (
                    Lwt_io.read_line_opt client.iChan >>= (
                        fun secret -> (
                            match secret with
                            | Some secret -> (
                                if (secret = admin_SECRET) then (
                                    Lwt.on_failure (adminHandler client clientList ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
                                    Logs_lwt.info (fun m -> m "Admin Handshake Successful") >>= return
                                ) else (
                                    Logs_lwt.info (fun m -> m "Invalid Admin Password Provided. Connection Refused.") >>= return
                                )
                            )
                            | None ->  Logs_lwt.info (fun m -> m "Admin Password Was Not Provided. Connection Refused.") >>= return
                        )
                    )
                ) else (
                    Logs_lwt.info (fun m -> m "Unknown Client Failed Handshake") >>= return
                )
            | None -> Logs_lwt.info (fun m -> m "Unknown Client Disconnected PRE_HANDSHAKE") >>= return
    )


let server_port = 8484
let so_timeout = Some 20
let backlog = 10

let try_close chan =
  catch (fun () -> Lwt_io.close chan)
  (function _ -> return ())

let init_socket sockaddr =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr;
  Lwt_unix.listen socket backlog;
  socket



let process socket ~timeout ~callback =
  let rec _process clientList () =
    Lwt_unix.accept socket >>=
      (fun (socket_cli, _) ->
        let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket_cli in
        let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket_cli in
        let client = { iChan = inchan; oChan = outchan } in
        let c = callback client (client :: clientList) in
        let events =
          match timeout with
          | None -> [c]
          | Some t -> [c; Lwt_unix.sleep (float_of_int t) >>= fun () -> return ()]
        in
        ignore (Lwt.pick events >>= fun () -> try_close outchan >>= fun () -> try_close inchan);

        
        _process (client :: clientList) ()
      )
  in
  _process [] ()

let _ =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, server_port) in
  let socket = init_socket sockaddr in
  Lwt_main.run (
    process
      socket
      ~timeout:so_timeout
      ~callback:
        (fun client clientList ->
            Lwt.on_failure (handshakeHandler client clientList ();) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
            return ()
        )
  )