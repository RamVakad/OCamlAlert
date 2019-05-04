open Lwt
open Lwt_io

let alert = ref "Default Alert"

type client = {
    oChan : Lwt_io.output Lwt_io.channel;
    iChan : Lwt_io.input Lwt_io.channel
}

let clientList = ref []

let addClient client = clientList := [client] @ !clientList 

let rec sendToAll alert =
    List.iter (fun client -> Lwt_io.write_line client.oChan alert; ()) !clientList

let handle_message msg =
    match msg with
    | "getAlert" -> !alert
    | "setAlert"  -> alert := "New Alert"; sendToAll "HAHAHAHA"; "New Alert has been set."
    | a      -> print_string "Message Received: "; print_endline a; "Unknown command"

let rec handle_connection ic oc () =
    Lwt_io.read_line_opt ic >>=
    (fun msg ->
        match msg with
        | Some msg -> 
            let reply = handle_message msg in
            Lwt_io.write_line oc reply >>= handle_connection ic oc
        | None -> Logs_lwt.info (fun m -> m "One Alert Client Disconnected") >>= return)

let accept_connection conn =
    let fd, _ = conn in
    let ic = Lwt_io.of_fd Lwt_io.Input fd in
    let oc = Lwt_io.of_fd Lwt_io.Output fd in
    let client = { iChan = ic; oChan = oc } in

    Lwt.on_failure (handle_connection ic oc ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
    addClient client;
    Logs_lwt.info (fun m -> m "New Alert Client Connected") >>= return
 
let create_socket () =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock @@ ADDR_INET(Unix.inet_addr_loopback, 8484);
    listen sock 10;
    sock

let create_server sock =
    let rec serve () =
        Lwt_unix.accept sock >>= accept_connection >>= serve
    in serve

let () =
    let () = Logs.set_reporter (Logs.format_reporter ()) in
    let () = Logs.set_level (Some Logs.Info) in
    let sock = create_socket () in
    let serve = create_server sock in
    print_endline "OCamlAlert Server is online on port 8484.";
    Lwt_main.run @@ serve ();