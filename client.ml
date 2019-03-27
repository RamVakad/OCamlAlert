open Unix
open Printf
open Thread

let alert = ref "Default Alert"

let readStream inputStream =
    let rec readLine msg =
      if (msg = "[END]") then 
            (Pervasives.print_endline "Received [END] from server.")
      else
        let line = (Pervasives.input_line inputStream) in
          (Pervasives.print_endline line; Thread.delay 0.5; readLine line)
    in readLine "[START]"

let loopAlerts sock =
    let inputStream = (Unix.in_channel_of_descr sock) in
    let outputStream = (Unix.out_channel_of_descr sock) in
        Printf.printf "Listening for alerts...\n";
        let rec getAlert msg = 
            if (msg = "[END]") then (
              Pervasives.print_endline "Received msg = [END] from server, aborting.";
            ) else (
                Printf.fprintf outputStream "getAlert\n";
                Pervasives.flush outputStream;
                Thread.delay 1.0;
                let line = (Pervasives.input_line inputStream) in (
                  
                  (Pervasives.print_endline line; Thread.delay 5.0; getAlert line)
                )
            )
        in getAlert "[START]"

let _ =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
    Unix.connect sock (ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 8484));
    Printf.printf "Connected to 127.0.0.1:8484...\n";
    loopAlerts sock;
    Printf.printf "Closing the connection.\n!";
    Unix.close sock