open Unix
open Printf
open Thread

let alert sock =
    let outputStream = (Unix.out_channel_of_descr sock) in
    Printf.fprintf outputStream "iAmAdmin\n";
    Pervasives.flush outputStream;
    Printf.fprintf outputStream "SuperSecretPassword\n";
    Pervasives.flush outputStream;
    Printf.fprintf outputStream "FLOOD ALERT STAY SAFE\n";
    Pervasives.flush outputStream;
    ()

let _ =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
    Unix.connect sock (ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 8484));
    Printf.printf "Connected to 127.0.0.1:8484...\n";
    alert sock;
    Printf.printf "Closing the connection.";
    Unix.close sock