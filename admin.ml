open Unix
open Printf
open Thread

let addLineBreak str = str ^ "\n"

let alert sock pass msg =
    let inputStream = (Unix.in_channel_of_descr sock) in
    let outputStream = (Unix.out_channel_of_descr sock) in
    Printf.fprintf outputStream "iAmAdmin\n";
    Pervasives.flush outputStream;
    Printf.fprintf outputStream "%s" (addLineBreak pass);
    Pervasives.flush outputStream;
    Printf.fprintf outputStream "%s" (addLineBreak msg);
    Pervasives.flush outputStream;
    Thread.delay 2.0;
    let msg = (Pervasives.input_line inputStream) in (
      if (msg = "OK") then (Pervasives.print_endline "The alert has been successfully pushed to all clients.";) else (Pervasives.print_endline "Invalid Password.";);
    );
    
    ()

let getHead ls =
  match ls with
  | [] -> ""
  | a :: b -> a

let removeHead ls =
  match ls with
  | [] -> []
  | a :: b -> b

let getAlert ls = String.concat " " ls


let _ =
  let argList = Array.to_list Sys.argv in
  let pass = getHead (removeHead argList) in
  let msg = getAlert (removeHead (removeHead argList)) in
  let sock = Unix.socket PF_INET SOCK_STREAM 0 in
    Unix.connect sock (ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 8484));
    Printf.printf "Connected to 127.0.0.1:8484...\n";
    alert sock pass msg;
    Printf.printf "Closing the connection.\n";
    Unix.close sock