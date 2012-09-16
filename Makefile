
modules = llvm/Core llvm/Analysis main ast misc base/array base/sequence lexer base/stack tokenstream parser node/aggregate node/func node/funcinstance node/integer node/meta node/node node/type node/value codegen symboltable literal
objs = $(patsubst %, obj/%.o, $(modules))
source = $(patsubst %, %.d, $(modules))

all : bin/boa
cleanall: clean

obj/%.o : %.d
	dmd -g -debug -c -of$@ $<

bin/boa : $(objs)
	g++ -o $@ $^ -lphobos2 -lLLVM-3.0 -lpthread -lrt

clean :
	rm -fr obj/* bin/*

