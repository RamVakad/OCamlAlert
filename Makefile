all: server client

server: server.ml
	ocamlfind ocamlopt -package lwt,lwt.unix,logs,logs.lwt -linkpkg -o server ./server.ml
	-rm -r server.o server.cmx server.cmi

client: client.ml
	ocamlfind ocamlopt -package unix,threads -linkpkg -thread -o client ./client.ml
	-rm -r client.o client.cmx client.cmi

clean:
	-rm -r client server _build *.o *.cmo *.cmx *.mli *.cmi