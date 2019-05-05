all: server client admin

newserver: newserver.ml
	ocamlfind ocamlopt -package lwt,lwt.unix,logs,logs.lwt -linkpkg -o newserver ./newserver.ml
	-rm -r newserver.o newserver.cmx newserver.cmi

server: server.ml
	ocamlfind ocamlopt -package lwt,lwt.unix,logs,logs.lwt -linkpkg -o server ./server.ml
	-rm -r server.o server.cmx server.cmi

client: client.ml
	ocamlfind ocamlopt -package unix,threads -linkpkg -thread -o client ./client.ml
	-rm -r client.o client.cmx client.cmi

admin: admin.ml
	ocamlfind ocamlopt -package unix,threads -linkpkg -thread -o admin ./admin.ml
	-rm -r admin.o admin.cmx admin.cmi

clean:
	-rm -r client server _build *.o *.cmo *.cmx *.mli *.cmi