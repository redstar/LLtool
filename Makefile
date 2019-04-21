all: LLtool FileCheck

LLtool::
	dub build --build=unittest

FileCheck: utils/FileCheck/FileCheck.cpp
	clang++ $(shell llvm-config --cxxflags) -o FileCheck utils/FileCheck/FileCheck.cpp $(shell llvm-config --ldflags) $(shell llvm-config --libs support) $(llvm-config --system-libs) -lpthread

examples::
	cd examples/Oberon-2 && dub build

runtest:
	sed -e s:@LLTOOL_BIN_DIR@:$(PWD):g test/lit.site.cfg.in >test/lit.site.cfg
	test/runlit.py -vv test
