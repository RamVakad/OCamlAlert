open Unix
open Printf
open Thread

let readStream inputStream =
    let rec readLine msg =
      if (msg = "[END]") then 
            (Pervasives.print_endline "Received [END] from server.")
      else
        let line = (Pervasives.input_line inputStream) in
          (Pervasives.print_endline line; Thread.delay 0.5; readLine line)
    in readLine "[START]"

let _ =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
    Unix.connect sock (ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 8484));
    Printf.printf "Connected to localhost on 8484...\n%!";
    readStream (Unix.in_channel_of_descr sock);
    Printf.printf "Closing the connection.\n%!";
    Unix.close sock