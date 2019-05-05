open Unix
open Printf
open Thread
open Pervasives

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
    Printf.fprintf outputStream "iAmClient\n";
    Pervasives.flush outputStream;
    Pervasives.print_endline "Listening for alerts...";
    let rec keepAlive msg = (
      Pervasives.print_endline ".";
      Printf.fprintf outputStream "KEEP-ALIVE\n";
      Pervasives.flush outputStream;
      Thread.delay 5.0;
      let msg = (Pervasives.input_line inputStream) in (
        if (msg <> "KEEP-ALIVE") then ( Pervasives.print_endline msg;);
        keepAlive "MSG"
      )
    ) in keepAlive "MSG";
    "RETURN"

let _ =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
    Unix.connect sock (ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 8484));
    Pervasives.print_endline "Connected to 127.0.0.1:8484...";
    loopAlerts sock;
    Pervasives.print_endline "Closing the connection.";
    Unix.close sock