
PLOT_FILE := bench.png
.PHONY : opt bench clean install prof test debug benchprof threadscope $(PLOT_FILE) bench-TrieBench.csv BenchmarkP.prof

FAST_DIR := out/fast
OPTIMIZED_DIR := out/opt
GHC_OPTS := -Wall -fno-warn-name-shadowing -fno-warn-orphans -rtsopts
FAST_GHC_OPTS := -O0 -ddump-minimal-imports -odir $(FAST_DIR) $(GHC_OPTS)
DEBUG_GHC_OPTS := -prof -hisuf p_hi -auto-all  -rtsopts -osuf p_o  $(FAST_GHC_OPTS) $(GHC_OPTS)
LLVM_OPTS := -O3 -std-compile-opts
OPTIMIZED_GHC_OPTS := -O2 -fno-spec-constr-count -fno-spec-constr-threshold \
  -fllvm $(addprefix -optlo, $(LLVM_OPTS)) \
  -fmax-worker-args=100 -funfolding-keeness-factor=100 -odir $(OPTIMIZED_DIR) $(GHC_OPTS)
THREADSCOPE_OPTS := $(OPTIMIZED_GHC_OPTS) $(GHC_OPTS) -eventlog
PROFILING_OPTS := -prof -hisuf p_hi -auto-all -rtsopts -osuf p_o $(OPTIMIZED_GHC_OPTS) $(GHC_OPTS)
HP2PS_OPTS := -c -s -m12 -d
RTS_OPTS := -H256M -A32M -s
PROGRESSION_PREFIXES := ""
PROGRESSION_GROUP := normal-bench

fast : $(FAST_DIR)/Data/TrieSet.o $(FAST_DIR)/Data/TrieMap.o
debug: $(FAST_DIR)/Data/TrieSet.p_o $(FAST_DIR)/Data/TrieMap.p_o
opt : $(OPTIMIZED_DIR)/Data/TrieSet.o $(OPTIMIZED_DIR)/Data/TrieMap.o
prof : $(OPTIMIZED_DIR)/Data/TrieSet.p_o $(OPTIMIZED_DIR)/Data/TrieMap.p_o
install : test
	cabal install --enable-documentation

test : Tests
	./Tests

testdbg :: TestsP
	./TestsP +RTS -xc

bench : SAMPLES = 30
bench : $(PLOT_FILE)

TRIEBENCH_OPTS = --name="TrieBench" --mode=run --group=$(PROGRESSION_GROUP) --prefixes=$(PROGRESSION_PREFIXES) \
		--compare="" -- -s $(SAMPLES)

bench.png : bench-TrieBench.csv bench-SetBench.csv
	./TrieBench --mode=graph --group=$(PROGRESSION_GROUP) --compare="TrieBench,SetBench" --plot=$(PLOT_FILE) \
		--plot-log-y

bench-TrieBench.csv : TrieBench
	./TrieBench +RTS $(RTS_OPTS) -RTS $(TRIEBENCH_OPTS)

bench-SetBench.csv : SetBench
	./SetBench +RTS $(RTS_OPTS) -RTS --name="SetBench" --mode=run --group=bench --prefixes=$(PROGRESSION_PREFIXES) \
		--compare="" -- -s $(SAMPLES)

benchprof : BenchmarkP.prof BenchmarkP.ps
	less BenchmarkP.prof

threadscope: Benchlog.eventlog
	threadscope Benchlog.eventlog

BenchmarkP.ps : BenchmarkP.hp
	hp2ps $(HP2PS_OPTS) $<

BenchmarkP.hp : BenchmarkP.prof

BenchmarkP.prof : SAMPLES  = 5
BenchmarkP.prof : BenchmarkP
	./BenchmarkP +RTS -P -hd $(RTS_OPTS) -RTS $(TRIEBENCH_OPTS)

Benchlog.eventlog : SAMPLES = 10
Benchlog.eventlog : Benchlog
	./Benchlog +RTS $(RTS_OPTS) -ls -RTS $(TRIEBENCH_OPTS)

Tests : fast
	ghc $(FAST_GHC_OPTS) Tests -o Tests -main-is Tests.main

TestsP : fast debug
	ghc $(DEBUG_GHC_OPTS) Tests -o TestsP -main-is Tests.main

BenchmarkP : opt prof
	ghc $(PROFILING_OPTS) -w TrieBench -o BenchmarkP -main-is TrieBench.main

Benchmark : opt
	ghc $(OPTIMIZED_GHC_OPTS) -w TrieBench -o Benchmark -main-is TrieBench.main

TrieBench : TrieBench.hs opt
	ghc $(OPTIMIZED_GHC_OPTS) -w $< -o $@ -main-is TrieBench.main
SetBench : SetBench.hs opt
	ghc $(OPTIMIZED_GHC_OPTS) -w $< -o $@ -main-is SetBench.main

Benchlog : opt
	ghc $(THREADSCOPE_OPTS) -w TrieBench -o Benchlog -main-is TrieBench.main

clean:
	rm -f *.imports
	rm -rf out/
	rm -f SetBench TrieBench BenchmarkP Tests
	rm -f bench-SetBench.csv bench-TrieBench.csv bench.csv bench.png
	rm -f *.o *.hi

# DO NOT DELETE: Beginning of Haskell dependencies
$(FAST_DIR)/Data/TrieMap/Modifiers.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o

$(FAST_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Utils.o
$(FAST_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o

$(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.o : $(FAST_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o
$(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Utils.o
$(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.o : $(FAST_DIR)/Data/TrieMap/Modifiers.o

$(FAST_DIR)/Data/TrieMap/Representation/TH/Factorized.o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Utils.o
$(FAST_DIR)/Data/TrieMap/Representation/TH/Factorized.o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.o

$(FAST_DIR)/Data/TrieMap/Representation/TH.o : $(FAST_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o
$(FAST_DIR)/Data/TrieMap/Representation/TH.o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Factorized.o
$(FAST_DIR)/Data/TrieMap/Representation/TH.o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.o
$(FAST_DIR)/Data/TrieMap/Representation/TH.o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Utils.o
$(FAST_DIR)/Data/TrieMap/Representation/TH.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o

$(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.o : $(FAST_DIR)/Data/TrieMap/Representation/TH.o


$(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.o : $(FAST_DIR)/Data/TrieMap/Utils.o

$(FAST_DIR)/Data/TrieMap/Representation/Instances/ByteString.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/ByteString.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o

$(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o

$(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(FAST_DIR)/Data/TrieMap/Representation/TH.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.o


$(FAST_DIR)/Data/TrieMap/Representation/Instances.o : $(FAST_DIR)/Data/TrieMap/Representation/TH.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/ByteString.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.o : $(FAST_DIR)/Data/TrieMap/Utils.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.o : $(FAST_DIR)/Data/TrieMap/Modifiers.o

$(FAST_DIR)/Data/TrieMap/Representation.o : $(FAST_DIR)/Data/TrieMap/Representation/TH.o
$(FAST_DIR)/Data/TrieMap/Representation.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances.o
$(FAST_DIR)/Data/TrieMap/Representation.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o


$(FAST_DIR)/Data/TrieMap/TrieKey.o : $(FAST_DIR)/Control/Monad/Ends.o
$(FAST_DIR)/Data/TrieMap/TrieKey.o : $(FAST_DIR)/Data/TrieMap/Utils.o
$(FAST_DIR)/Data/TrieMap/TrieKey.o : $(FAST_DIR)/Data/TrieMap/Sized.o

$(FAST_DIR)/Data/TrieMap/Class.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/Class.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Class.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o

$(FAST_DIR)/Data/TrieMap/ProdMap.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap/ProdMap.o : $(FAST_DIR)/Data/TrieMap/Sized.o

$(FAST_DIR)/Data/TrieMap/UnitMap.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/UnitMap.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o

$(FAST_DIR)/Data/TrieMap/UnionMap.o : $(FAST_DIR)/Data/TrieMap/UnitMap.o
$(FAST_DIR)/Data/TrieMap/UnionMap.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/UnionMap.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o

$(FAST_DIR)/Data/TrieMap/OrdMap.o : $(FAST_DIR)/Data/TrieMap/Modifiers.o
$(FAST_DIR)/Data/TrieMap/OrdMap.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/OrdMap.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o

$(FAST_DIR)/Data/TrieMap/Key.o : $(FAST_DIR)/Data/TrieMap/Modifiers.o
$(FAST_DIR)/Data/TrieMap/Key.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Key.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/Key.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap/Key.o : $(FAST_DIR)/Data/TrieMap/Class.o

$(FAST_DIR)/Data/TrieMap/WordMap.o : $(FAST_DIR)/Data/TrieMap/Utils.o
$(FAST_DIR)/Data/TrieMap/WordMap.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/WordMap.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o

$(FAST_DIR)/Data/TrieMap/RadixTrie/Label.o : $(FAST_DIR)/Data/TrieMap/WordMap.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Label.o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Slice.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Label.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Label.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o

$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Slice.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Label.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(FAST_DIR)/Data/TrieMap/WordMap.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(FAST_DIR)/Data/TrieMap/Sized.o

$(FAST_DIR)/Data/TrieMap/RadixTrie.o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Label.o
$(FAST_DIR)/Data/TrieMap/RadixTrie.o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.o
$(FAST_DIR)/Data/TrieMap/RadixTrie.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/RadixTrie.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o

$(FAST_DIR)/Data/TrieMap/ReverseMap.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/ReverseMap.o : $(FAST_DIR)/Data/TrieMap/Modifiers.o
$(FAST_DIR)/Data/TrieMap/ReverseMap.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap/ReverseMap.o : $(FAST_DIR)/Control/Monad/Ends.o

$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/Key.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/UnitMap.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/UnionMap.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/ProdMap.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/OrdMap.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/WordMap.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/RadixTrie.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/ReverseMap.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/Class.o

$(FAST_DIR)/Data/TrieSet.o : $(FAST_DIR)/Control/Monad/Ends.o
$(FAST_DIR)/Data/TrieSet.o : $(FAST_DIR)/Data/TrieMap/Utils.o
$(FAST_DIR)/Data/TrieSet.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieSet.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieSet.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieSet.o : $(FAST_DIR)/Data/TrieMap/Class/Instances.o
$(FAST_DIR)/Data/TrieSet.o : $(FAST_DIR)/Data/TrieMap/Class.o

$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Utils.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Representation.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Class/Instances.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Class.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Control/Monad/Ends.o

# OPT

$(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Factorized.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Factorized.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Factorized.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o


$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/ByteString.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/ByteString.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.o


$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/ByteString.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o


$(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o : $(OPTIMIZED_DIR)/Control/Monad/Ends.o
$(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o

$(OPTIMIZED_DIR)/Data/TrieMap/Class.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o

$(OPTIMIZED_DIR)/Data/TrieMap/ProdMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap/ProdMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o

$(OPTIMIZED_DIR)/Data/TrieMap/UnitMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/UnitMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o

$(OPTIMIZED_DIR)/Data/TrieMap/UnionMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/UnitMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/UnionMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/UnionMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o

$(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.o
$(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o

$(OPTIMIZED_DIR)/Data/TrieMap/Key.o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.o

$(OPTIMIZED_DIR)/Data/TrieMap/WordMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap/WordMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/WordMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o

$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.o : $(OPTIMIZED_DIR)/Data/TrieMap/WordMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Slice.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o

$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Slice.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(OPTIMIZED_DIR)/Data/TrieMap/WordMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o

$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o

$(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.o
$(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.o : $(OPTIMIZED_DIR)/Control/Monad/Ends.o

$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Key.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/UnitMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/UnionMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/ProdMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/WordMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.o

$(OPTIMIZED_DIR)/Data/TrieSet.o : $(OPTIMIZED_DIR)/Control/Monad/Ends.o
$(OPTIMIZED_DIR)/Data/TrieSet.o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.o
$(OPTIMIZED_DIR)/Data/TrieSet.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieSet.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieSet.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieSet.o : $(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o
$(OPTIMIZED_DIR)/Data/TrieSet.o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.o

$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Control/Monad/Ends.o

# PROF

$(FAST_DIR)/Data/TrieMap/Modifiers.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o

$(FAST_DIR)/Data/TrieMap/Representation/TH/ReprMonad.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Utils.p_o
$(FAST_DIR)/Data/TrieMap/Representation/TH/ReprMonad.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o

$(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH/ReprMonad.p_o
$(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Utils.p_o
$(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o
$(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.p_o : $(FAST_DIR)/Data/TrieMap/Modifiers.p_o

$(FAST_DIR)/Data/TrieMap/Representation/TH/Factorized.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Utils.p_o
$(FAST_DIR)/Data/TrieMap/Representation/TH/Factorized.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.p_o

$(FAST_DIR)/Data/TrieMap/Representation/TH.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH/ReprMonad.p_o
$(FAST_DIR)/Data/TrieMap/Representation/TH.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Factorized.p_o
$(FAST_DIR)/Data/TrieMap/Representation/TH.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Representation.p_o
$(FAST_DIR)/Data/TrieMap/Representation/TH.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Utils.p_o
$(FAST_DIR)/Data/TrieMap/Representation/TH.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o

$(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH.p_o


$(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.p_o : $(FAST_DIR)/Data/TrieMap/Utils.p_o

$(FAST_DIR)/Data/TrieMap/Representation/Instances/ByteString.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/ByteString.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o

$(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o

$(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.p_o


$(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/ByteString.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Utils.p_o
$(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Modifiers.p_o

$(FAST_DIR)/Data/TrieMap/Representation.p_o : $(FAST_DIR)/Data/TrieMap/Representation/TH.p_o
$(FAST_DIR)/Data/TrieMap/Representation.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o
$(FAST_DIR)/Data/TrieMap/Representation.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o


$(FAST_DIR)/Data/TrieMap/TrieKey.p_o : $(FAST_DIR)/Control/Monad/Ends.p_o
$(FAST_DIR)/Data/TrieMap/TrieKey.p_o : $(FAST_DIR)/Data/TrieMap/Utils.p_o
$(FAST_DIR)/Data/TrieMap/TrieKey.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o

$(FAST_DIR)/Data/TrieMap/Class.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap/Class.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o
$(FAST_DIR)/Data/TrieMap/Class.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o

$(FAST_DIR)/Data/TrieMap/ProdMap.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o
$(FAST_DIR)/Data/TrieMap/ProdMap.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o

$(FAST_DIR)/Data/TrieMap/UnitMap.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap/UnitMap.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o

$(FAST_DIR)/Data/TrieMap/UnionMap.p_o : $(FAST_DIR)/Data/TrieMap/UnitMap.p_o
$(FAST_DIR)/Data/TrieMap/UnionMap.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap/UnionMap.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o

$(FAST_DIR)/Data/TrieMap/OrdMap.p_o : $(FAST_DIR)/Data/TrieMap/Modifiers.p_o
$(FAST_DIR)/Data/TrieMap/OrdMap.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap/OrdMap.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o

$(FAST_DIR)/Data/TrieMap/Key.p_o : $(FAST_DIR)/Data/TrieMap/Modifiers.p_o
$(FAST_DIR)/Data/TrieMap/Key.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o
$(FAST_DIR)/Data/TrieMap/Key.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap/Key.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o
$(FAST_DIR)/Data/TrieMap/Key.p_o : $(FAST_DIR)/Data/TrieMap/Class.p_o

$(FAST_DIR)/Data/TrieMap/WordMap.p_o : $(FAST_DIR)/Data/TrieMap/Utils.p_o
$(FAST_DIR)/Data/TrieMap/WordMap.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap/WordMap.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o

$(FAST_DIR)/Data/TrieMap/RadixTrie/Label.p_o : $(FAST_DIR)/Data/TrieMap/WordMap.p_o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Label.p_o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Slice.p_o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Label.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Label.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o

$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.p_o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Slice.p_o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.p_o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Label.p_o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.p_o : $(FAST_DIR)/Data/TrieMap/WordMap.p_o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o

$(FAST_DIR)/Data/TrieMap/RadixTrie.p_o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Label.p_o
$(FAST_DIR)/Data/TrieMap/RadixTrie.p_o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.p_o
$(FAST_DIR)/Data/TrieMap/RadixTrie.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap/RadixTrie.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o

$(FAST_DIR)/Data/TrieMap/ReverseMap.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap/ReverseMap.p_o : $(FAST_DIR)/Data/TrieMap/Modifiers.p_o
$(FAST_DIR)/Data/TrieMap/ReverseMap.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o
$(FAST_DIR)/Data/TrieMap/ReverseMap.p_o : $(FAST_DIR)/Control/Monad/Ends.p_o

$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Key.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/UnitMap.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/UnionMap.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/ProdMap.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/OrdMap.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/WordMap.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/RadixTrie.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/ReverseMap.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o
$(FAST_DIR)/Data/TrieMap/Class/Instances.p_o : $(FAST_DIR)/Data/TrieMap/Class.p_o

$(FAST_DIR)/Data/TrieSet.p_o : $(FAST_DIR)/Control/Monad/Ends.p_o
$(FAST_DIR)/Data/TrieSet.p_o : $(FAST_DIR)/Data/TrieMap/Utils.p_o
$(FAST_DIR)/Data/TrieSet.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieSet.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Class.p_o
$(FAST_DIR)/Data/TrieSet.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o
$(FAST_DIR)/Data/TrieSet.p_o : $(FAST_DIR)/Data/TrieMap/Class/Instances.p_o
$(FAST_DIR)/Data/TrieSet.p_o : $(FAST_DIR)/Data/TrieMap/Class.p_o

$(FAST_DIR)/Data/TrieMap.p_o : $(FAST_DIR)/Data/TrieMap/Utils.p_o
$(FAST_DIR)/Data/TrieMap.p_o : $(FAST_DIR)/Data/TrieMap/Sized.p_o
$(FAST_DIR)/Data/TrieMap.p_o : $(FAST_DIR)/Data/TrieMap/Representation/Instances.p_o
$(FAST_DIR)/Data/TrieMap.p_o : $(FAST_DIR)/Data/TrieMap/Representation.p_o
$(FAST_DIR)/Data/TrieMap.p_o : $(FAST_DIR)/Data/TrieMap/TrieKey.p_o
$(FAST_DIR)/Data/TrieMap.p_o : $(FAST_DIR)/Data/TrieMap/Class/Instances.p_o
$(FAST_DIR)/Data/TrieMap.p_o : $(FAST_DIR)/Data/TrieMap/Class.p_o
$(FAST_DIR)/Data/TrieMap.p_o : $(FAST_DIR)/Control/Monad/Ends.p_o

# OPT

$(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/ReprMonad.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Utils.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/ReprMonad.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/ReprMonad.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Utils.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Factorized.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Utils.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Factorized.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/ReprMonad.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Factorized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Representation.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Utils.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.p_o


$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/ByteString.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/ByteString.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.p_o


$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/ByteString.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Representation.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o


$(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o : $(OPTIMIZED_DIR)/Control/Monad/Ends.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Class.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/ProdMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/ProdMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/UnitMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/UnitMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/UnionMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/UnitMap.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/UnionMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/UnionMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Key.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/WordMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/WordMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/WordMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/WordMap.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Slice.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Slice.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/WordMap.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Label.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.p_o : $(OPTIMIZED_DIR)/Control/Monad/Ends.p_o

$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Key.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/UnitMap.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/UnionMap.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/ProdMap.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/WordMap.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.p_o

$(OPTIMIZED_DIR)/Data/TrieSet.p_o : $(OPTIMIZED_DIR)/Control/Monad/Ends.p_o
$(OPTIMIZED_DIR)/Data/TrieSet.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.p_o
$(OPTIMIZED_DIR)/Data/TrieSet.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieSet.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.p_o
$(OPTIMIZED_DIR)/Data/TrieSet.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o
$(OPTIMIZED_DIR)/Data/TrieSet.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o
$(OPTIMIZED_DIR)/Data/TrieSet.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.p_o

$(OPTIMIZED_DIR)/Data/TrieMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.p_o
$(OPTIMIZED_DIR)/Data/TrieMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.p_o
$(OPTIMIZED_DIR)/Data/TrieMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.p_o
$(OPTIMIZED_DIR)/Data/TrieMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation.p_o
$(OPTIMIZED_DIR)/Data/TrieMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.p_o
$(OPTIMIZED_DIR)/Data/TrieMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.p_o
$(OPTIMIZED_DIR)/Data/TrieMap.p_o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.p_o
$(OPTIMIZED_DIR)/Data/TrieMap.p_o : $(OPTIMIZED_DIR)/Control/Monad/Ends.p_o

# DO NOT DELETE: End of Haskell dependencies

%.p_o : %.o
$(OPTIMIZED_DIR)/%.o : %.hs
	ghc -c $(OPTIMIZED_GHC_OPTS) $<
$(FAST_DIR)/%.o : %.hs
	ghc -c $(FAST_GHC_OPTS) $<
$(OPTIMIZED_DIR)/%.p_o : %.hs
	ghc -c $(PROFILING_OPTS) $<
$(FAST_DIR)/%.p_o : %.hs
	ghc -c $(DEBUG_GHC_OPTS) $<
