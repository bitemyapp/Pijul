DEBUG=-DDEBUG
PP= #-pp "cpp -w $(DEBUG)" #"camlp4o pa_macro.cmo $(DEBUG)"
OCAMLOPT=ocamlopt $(PP)
OCAMLC=ocamlcp -P f $(PP)
OCAMLDOC=ocamldoc $(PP)
CFLAGS=-Wall

$DESTDIR ?= /usr/local

SRC=pijul.ml repository.ml remote.ml interaction.ml commands.ml main.ml
bindir ?= bin

pijul:$(SRC:.ml=.cmx) mdb.cmxa
	ocamlfind $(OCAMLOPT) -package cryptokit,yojson -o $@ -cclib -L. mdb.cmxa str.cmxa -linkpkg $(SRC:.ml=.cmx)


install:pijul
	install -m 755 pijul $(DESTDIR)/$(bindir)/pijul


pijul.byte:$(SRC:.ml=.cmo) mdb.cma
	ocamlfind $(OCAMLC) -package cryptokit,yojson -o $@ -cclib -L. mdb.cma str.cma -custom -linkpkg $(SRC:.ml=.cmo)


pijul.cmx:mdb.cmxa pijul.cmi
pijul.cmo:mdb.cma pijul.cmi
pijul.cmi:mdb.cmxa
remote.cmi:mdb.cmxa pijul.cmi
remote.cmx:mdb.cmxa remote.cmi pijul.cmx
remote.cmo:mdb.cma remote.cmi pijul.cmx
commands.cmx:pijul.cmx interaction.cmx remote.cmx
commands.cmo:pijul.cmo interaction.cmo remote.cmo
interaction.cmx:pijul.cmx
interaction.cmo:pijul.cmo
main.cmx:commands.cmx pijul.cmx
main.cmo:commands.cmo pijul.cmo
remote.cmx: repository.cmx remote.cmi

%.cmx:%.ml
	ocamlfind $(OCAMLOPT) -package cryptokit,yojson -c -w A -o $@ $<

%.cmo:%.ml
	ocamlfind $(OCAMLC) -package cryptokit,yojson -c -w A -o $@ $<

%.cmi:%.mli
	$(OCAMLC) -c -w A -o $@ $<

mdb.cmxa:mdb_constants.ml mdb.ml lmdb_stubs.o
	ocamlmklib -o mdb lmdb_stubs.o mdb_constants.ml mdb.ml -llmdb -linkall

mdb.cma:mdb_constants.ml mdb.ml lmdb_stubs.o
	ocamlmklib -o mdb lmdb_stubs.o mdb_constants.ml mdb.ml -llmdb -linkall

mdb_constants.ml:make_stubs
	bash make_stubs

lmdb_stubs.o:lmdb_stubs.c
	cc -fPIC -Wall -O3 -c -o $@ $<

.PHONY:doc remotedoc
doc:
	ocamldoc -html -d doc pijul.mli

remotedoc:doc
	rsync -r doc gitit@ovh:pijul/static/

perf:performance.ml
	ocamlopt -o $@ str.cmxa unix.cmxa $<

clean:
	rm -f *~ *.cm[oxai] *.cmxa *.o *.so *.a
	rm -Rf doc

TESTS=tests/basic.tested tests/basic2.tested tests/unrecord.tested tests/rollback.tested\
  tests/remove.tested tests/move.tested tests/move-push.tested tests/zombie.tested tests/conflict.tested\
  tests/linedel.tested tests/revert.tested tests/delconflict.tested tests/basic-put.tested tests/ls.tested



FAILING_TESTS=

shell-tests: pijul $(TESTS)

%.tested:%.sh pijul
	pijul=`pwd`/pijul bash $<
