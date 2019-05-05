open Lwt
open Lwt_io

(*
    Client Handling Code
    //Handler Ping Pongs a KEEP-ALIVE message and just keeps the connection alive.
*)

(* A client is just an network input stream and output stream *)
type client = {
    oChan : Lwt_io.output Lwt_io.channel;
    iChan : Lwt_io.input Lwt_io.channel
}

(* BEGIN IMPERATIVE CODE ---- THIS IS THE ONLY IMPERATIVE CODE  *)
let clientList = ref [] (* List of clients *)

let addClient client = clientList := [client] @ !clientList  (* Add a client to the list *)

(* Function to push an alert to everyone in the list. *)
let pushAlert alert = List.iter (fun client -> Lwt_io.write_line client.oChan alert; ()) !clientList
(* END IMPERATIVE CODE *)

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

let rec adminHandler client () =
    Lwt_io.read_line_opt client.iChan >>= (
        fun msg ->
            match msg with
            | Some msg -> pushAlert msg; Logs_lwt.info (fun m -> m "New Alert Pushed") >>= return
            | None -> Logs_lwt.info (fun m -> m "Admin Client Disconnected") >>= return
    )

(* -------------------------------------------------------------------------------------------------------- *)

(*  Function: newConnectionHandler()
    Accepts Parameters - { conn : The TCP/IP Connection Object }
    Behavior: Accesses the "file descriptor" of the connection and then extracts the input
                and output streams of the connection.
                The first message is read from the input stream and the handshake is initiated.
                If the first message is "iAmClient", the client is automatically added to the clientList and
                is passed onto the clientHandler.
                If the first message is "iAmAdmin", the second message is read and if it matches the Administator
                Secret Key, the connection is passed onto the adminHandler.
                If the wrong key is provided or an invalid message is received, the connection is dropped.    *)
let newConnectionHandler conn =
    let fd, sa = conn in
    let ic = Lwt_io.of_fd Lwt_io.Input fd in
    let oc = Lwt_io.of_fd Lwt_io.Output fd in
    Lwt_io.read_line_opt ic >>= (
        fun handshake ->
            match handshake with
            | Some handshake ->
                let client = { iChan = ic; oChan = oc } in
                if (handshake = "iAmClient") then (
                    addClient client;
                    Lwt.on_failure (clientHandler client ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
                    Logs_lwt.info (fun m -> m "New Alert Client Connected") >>= return
                ) else if (handshake = "iAmAdmin") then (
                    Lwt_io.read_line_opt ic >>= (
                        fun secret -> (
                            match secret with
                            | Some secret -> (
                                if (secret = admin_SECRET) then (
                                    Lwt.on_failure (adminHandler client ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
                                    Logs_lwt.info (fun m -> m "New Admin Client Connected") >>= return
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
 
(*  Function: runServer()
    Accepts Parameters - { sock: Unix Socket }
    Behavior: Indefintely accepts incoming connections on the given socket.
                Accepted connections are passed onto newConnectionHandler()     *)
let runServer sock =
    let rec serve () =
        Lwt_unix.accept sock >>= newConnectionHandler >>= serve
    in serve


(*  Function: createSocket()
    Behavior: Creates a Unix Socket on port 8484 and returns it.    *)
let createSocket () =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock @@ ADDR_INET(Unix.inet_addr_loopback, 8484);
    listen sock 10;
    sock

(* () -> Program Entry Point -> Configures logger, creates ServerSocket, & starts listening. *)
let () =
    let () = Logs.set_reporter (Logs.format_reporter ()) in
    let () = Logs.set_level (Some Logs.Info) in
    let sock = createSocket () in
    let serve = runServer sock in
    print_endline "OCamlAlert Server is online on port 8484.";
    Lwt_main.run @@ serve ();