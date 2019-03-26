open Unix
open Printf
open Thread

let connThread sock =
  let outputStream = Unix.out_channel_of_descr sock in
    let rec sendMessages n =
      match n with
      | 10 -> Printf.fprintf outputStream "[END]\n"
      | _ -> Printf.fprintf outputStream "Hello, this is message %d.\n%!" (n + 1); Thread.delay 0.5; sendMessages (n + 1)
    in sendMessages 0;

    Printf.printf "Closing connection.\n%!";
    Pervasives.flush outputStream;
    Unix.close sock

let rec acceptLoop sock =
  let (s, _) = Unix.accept sock in
    printf "Accepted a connection.\n%!";
    let _ = Thread.create connThread s in acceptLoop sock

let _ =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
    Unix.setsockopt sock SO_REUSEADDR true;
    Unix.bind sock (ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 8484));
    Unix.listen sock 20;
    Printf.printf "Server is listening on 8484...\n%!";
    acceptLoop sock