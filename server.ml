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
        Lwt.catch ( fun () -> 
            Lwt_io.write_line client.oChan alert;
            Lwt.return_nil
        )
        (   function
            | Not_found -> print_endline (": Host not found"); Lwt.return_nil
            | e -> print_endline (": Host not found"); Lwt.return_nil
        );
        pushAlert alert tl
    | [] -> ()

let rec adminHandler client clientList () =
    Lwt_io.read_line_opt client.iChan >>= (
        fun msg ->
            match msg with
            | Some msg -> pushAlert ("[ALERT] " ^ msg) clientList; Logs_lwt.info (fun m -> m "New Alert Pushed To All Receivers") >>= return
            | None -> return ()
    )

(* -------------------------------------------------------------------------------------------------------- *)

(*  Function: handshakeHandler()
    Accepts Parameters - { conn : The TCP/IP Connection Object }
    Behavior: 
                The first message is read from the input stream and the handshake is initiated.
                If the first message is "iAmClient", the client is automatically added to the clientList and
                is passed onto the clientHandler.
                If the first message is "iAmAdmin", the second message is read and if it matches the Administator
                Secret Key, the connection is passed onto the adminHandler.
                If the wrong key is provided or an invalid message is received, the connection is dropped.    *)

let rec handshakeHandler client clientList () =
    Lwt_io.read_line_opt client.iChan >>= (
        fun firstMessage ->
            match firstMessage with
            | Some firstMessage ->
                if (firstMessage = "iAmClient") then (
                    print_endline "server: [INFO] Client Identified as Alert Receiver";
                    Lwt.on_failure (clientHandler client ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
                    Logs_lwt.info (fun m -> m "Client Handshake Successful") >>= return
                ) else if (firstMessage = "iAmAdmin") then (
                    print_endline "server: [INFO] Client Identified as Admin";
                    Lwt_io.read_line_opt client.iChan >>= (
                        fun secret -> (
                            match secret with
                            | Some secret -> (
                                if (secret = admin_SECRET) then (
                                    print_endline "server: [INFO] Admin Handshake Successful";
                                    Lwt.on_failure (adminHandler client clientList ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
                                    Lwt_io.write_line client.oChan "OK" >>= return;
                                    return ()
                                ) else (
                                    Lwt_io.write_line client.oChan "FAIL" >>= return;
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
                Accesses the "file descriptor" of the connection and then extracts the input
                and output streams of the connection.
                Creates a client, adds it to the list and the connection is
                passed onto handshakeHandler()     *)
let runServer sock =
    let rec acceptLoop clientList () =
        Lwt_unix.accept sock >>= (
            fun conn ->
            let fd, sa = conn in
            let ic = Lwt_io.of_fd Lwt_io.Input fd in
            let oc = Lwt_io.of_fd Lwt_io.Output fd in
            let client = { iChan = ic; oChan = oc } in
            Logs_lwt.info (fun m -> m "New Connection Initiated.") >>= (
                fun () -> 
                Lwt.on_failure (handshakeHandler client clientList ();) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
                return client)
        )
        >>= 
        (fun client -> acceptLoop (client :: clientList) ())
    in acceptLoop []

(*  Function: createSocket()
    Behavior: Creates a Unix Socket on port 8484 and returns it.    *)
let createSocket () =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock @@ ADDR_INET(Unix.inet_addr_loopback, 8484);
    listen sock 500;
    sock

(* () -> Program Entry Point -> Configures logger, creates ServerSocket, & starts listening. *)
let () =
    let () = Logs.set_reporter (Logs.format_reporter ()) in
    let () = Logs.set_level (Some Logs.Info) in
    let sock = createSocket () in
    let serve = runServer sock in
    print_endline "OCamlAlert Server is online on port 8484.";
    async_exception_hook.contents <- (fun e ->
    Lwt_io.printf "Got exception: %s\n" (Printexc.to_string e) |> Lwt.ignore_result);
    ignore (Lwt_main.run @@ serve ())