# Make sure ocamlbuild can find opam-managed packages: first run
#
# eval `opam config env`

# Easiest way to build: using ocamlbuild, which in turn uses ocamlfind
all : theatr.native queue.o
theatr.native :
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.bitreader,llvm.linker -cflags -w,+a-4 \
		theatr.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff theatr scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o
	rm -rf *.ll *.ll	
	rm -rf *.ll *.temp *.out *.err *.s *.exe	


# More detailed: build using ocamlc/ocamlopt + ocamlfind to locate LLVM

OBJS = ast.cmx codegen.cmx parser.cmx scanner.cmx semant.cmx theatr.cmx
POBJS = scanner.cmo parser.cmo ast.cmo semant.cmo print_ast.cmo 

theatr : $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis $(OBJS) -o theatr

print_ast : $(POBJS)
	ocamlc -o print_ast $(POBJS)

clean_ast :
	rm -f print_ast parser.ml parser.mli scanner.ml *.cmo *.cmi

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<

queue : queue.c
	cc -o queue -DBUILD_TEST queue.c

### Generated by "ocamldep *.ml *.mli" after building scanner.ml and parser.ml
ast.cmo :
ast.cmx :
codegen.cmo : ast.cmo
codegen.cmx : ast.cmx
theatr.cmo : semant.cmo scanner.cmo parser.cmi codegen.cmo ast.cmo
theatr.cmx : semant.cmx scanner.cmx parser.cmx codegen.cmx ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
semant.cmo : ast.cmo
semant.cmx : ast.cmx
parser.cmi : ast.cmo

# Building the tarball

TESTS = arith1 arith2 arith3 fib for1 for2 func1 func2 func3 func4	\
    func5 gcd2 gcd global1 global2 hello if1 if2 if3 if4 ops1 ops2	\
    var1 while1 local1

FAILS = assign1 assign2 assign3 dead1 dead2 expr1 expr2 for1 for2	\
    for3 for4 for5 func1 func2 func3 func4 func5 func6 func7 func8	\
    func9 global1 global2 if1 if2 if3 nomain return1 return2 while1	\
    while2

TESTFILES = $(TESTS:%=test-%.mc) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.mc) $(FAILS:%=fail-%.err)

TARFILES = ast.ml codegen.ml Makefile theatr.ml parser.mly README scanner.mll \
	semant.ml testall.sh $(TESTFILES:%=tests/%)

microc-llvm.tar.gz : $(TARFILES)
	cd .. && tar czf microc-llvm/microc-llvm.tar.gz \
		$(TARFILES:%=microc-llvm/%)
