all: LLtool FileCheck

LLtool::
	dub build

FileCheck: utils/FileCheck/FileCheck.cpp
	clang++ $(shell llvm-config-8 --cxxflags) -o FileCheck utils/FileCheck/FileCheck.cpp $(shell llvm-config-8 --ldflags) $(shell llvm-config-8 --libs support) $(llvm-config-8 --system-libs) -lpthread

examples::
	cd examples/Oberon-2 && dub build

runtest:
	sed -e s:@LLTOOL_BIN_DIR@:$(PWD):g test/lit.site.cfg.in >test/lit.site.cfg
	test/runlit.py -vv test
