all: SimpleServer.native SimpleClient.native

clean:
	ocamlbuild -clean

%.native: %.ml
	ocamlbuild $@