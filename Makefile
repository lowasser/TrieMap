
PLOT_FILE := bench.png
.PHONY : opt bench clean install prof test debug benchprof threadscope $(PLOT_FILE) BenchmarkP.prof intbench

GHC_BIN := ghc-7.0.1
FAST_DIR := out/fast
OPTIMIZED_DIR := out/opt
GHC_OPTS := -Wall -fno-warn-name-shadowing -fno-warn-orphans -rtsopts $(EXTRA_OPTS)
FAST_GHC_OPTS := -O0 -ddump-minimal-imports -odir $(FAST_DIR) $(GHC_OPTS)
DEBUG_GHC_OPTS := -prof -hisuf p_hi -auto-all  -rtsopts -osuf p_o  $(FAST_GHC_OPTS) $(GHC_OPTS)
LLVM_OPTS := -O3 -std-compile-opts -partialspecialization -stats
OPTIMIZED_GHC_OPTS := -O2 -fno-spec-constr-count -fno-spec-constr-threshold \
  -fllvm $(addprefix -optlo, $(LLVM_OPTS)) \
  -fmax-worker-args=100 -funfolding-keeness-factor=100 -odir $(OPTIMIZED_DIR) $(GHC_OPTS)
THREADSCOPE_OPTS := $(OPTIMIZED_GHC_OPTS) $(GHC_OPTS) -eventlog
PROFILING_OPTS := -prof -hisuf p_hi -auto-all -rtsopts -osuf p_o $(OPTIMIZED_GHC_OPTS) $(GHC_OPTS)
HP2PS_OPTS := -c -s -m12 -d
RTS_OPTS := -H256M -A32M -s -M1G
PROGRESSION_PREFIXES := ""
PROGRESSION_GROUP := normal-bench

fast :
	$(GHC_BIN) $(FAST_GHC_OPTS) Data.TrieMap Data.TrieSet
debug:
	$(GHC_BIN) $(DEBUG_GHC_OPTS) Data.TrieMap Data.TrieSet
opt : 
	$(GHC_BIN) $(OPTIMIZED_GHC_OPTS) Data.TrieMap Data.TrieSet
prof :
	$(GHC_BIN) $(PROFILING_OPTS) Data.TrieMap Data.TrieSet
install : test
	cabal install --enable-documentation

test : Tests
	./Tests

testdbg :: TestsP
	./TestsP +RTS -xc

bench : SAMPLES = 30
bench : $(PLOT_FILE)

intbench : SAMPLES = 30
intbench : bench-IntBench-Trie.csv bench-IntBench-Set.csv bench-IntBench-IntSet.csv
	./IntBench-Trie --mode=graph --group=$(PROGRESSION_GROUP) --compare="IntBench-Trie,IntBench-Set,IntBench-IntSet" --plot=intbench.png \
		--plot-log-y

bench-IntBench-Trie.csv : IntBench-Trie
	./$< +RTS $(RTS_OPTS) -RTS --name="$<" $(BENCH_OPTS)

bench-IntBench-Set.csv : IntBench-Set
	./$< +RTS $(RTS_OPTS) -RTS --name="$<" $(BENCH_OPTS)

bench-IntBench-IntSet.csv : IntBench-IntSet
	./$< +RTS $(RTS_OPTS) -RTS --name="$<" $(BENCH_OPTS)

IntBench-Trie : opt IntBench/Trie.hs IntBench/Base.hs
	$(GHC_BIN) $(OPTIMIZED_GHC_OPTS) -w IntBench.Trie -main-is IntBench.Trie -o IntBench-Trie

IntBench-Set : IntBench/Set.hs IntBench/Base.hs
	$(GHC_BIN) $(OPTIMIZED_GHC_OPTS) -w IntBench.Set -main-is IntBench.Set -o IntBench-Set

IntBench-IntSet : IntBench/IntSet.hs IntBench/Base.hs
	$(GHC_BIN) $(OPTIMIZED_GHC_OPTS) -w IntBench.IntSet -main-is IntBench.IntSet -o IntBench-IntSet

BENCH_OPTS = --mode=run --group=$(PROGRESSION_GROUP) --prefixes=$(PROGRESSION_PREFIXES) \
		--compare="" -- -s $(SAMPLES)

bench.png : bench-TrieBench.csv bench-SetBench.csv
	./TrieBench --mode=graph --group=$(PROGRESSION_GROUP) --compare="TrieBench,SetBench" --plot=$@ \
		--plot-log-y

benchbs.png : SAMPLES = 30
benchbs.png : bench-TrieBench.csv bench-SetBench.csv bench-BSTrieBench.csv
	./TrieBench --mode=graph --group=$(PROGRESSION_GROUP) --compare="TrieBench,SetBench,BSTrieBench" --plot=$@ \
		--plot-log-y

bench-TrieBench.csv : TrieBench
	./TrieBench +RTS $(RTS_OPTS) -RTS --name="TrieBench" $(BENCH_OPTS)

bench-SetBench.csv : SetBench
	./SetBench +RTS $(RTS_OPTS) -RTS --name="SetBench" $(BENCH_OPTS)

bench-BSTrieBench.csv : BSTrieBench
	./BSTrieBench +RTS $(RTS_OPTS) -RTS --name="BSTrieBench" $(BENCH_OPTS)

benchprof : BenchmarkP.prof BenchmarkP.ps
	less BenchmarkP.prof

threadscope: Benchlog.eventlog
	threadscope Benchlog.eventlog

BenchmarkP.ps : BenchmarkP.hp
	hp2ps $(HP2PS_OPTS) $<

BenchmarkP.hp : BenchmarkP.prof

BenchmarkP.prof : SAMPLES  = 5
BenchmarkP.prof : BenchmarkP
	./BenchmarkP +RTS -P -hd $(RTS_OPTS) -RTS --name="BenchmarkP" $(BENCH_OPTS)

Benchlog.eventlog : SAMPLES = 10
Benchlog.eventlog : Benchlog
	./Benchlog +RTS $(RTS_OPTS) -ls -RTS $(TRIEBENCH_OPTS)

Tests : fast
	$(GHC_BIN) $(FAST_GHC_OPTS) Data.TrieMap.Tests -o Tests -main-is Data.TrieMap.Tests

TestsP : fast debug
	$(GHC_BIN) $(DEBUG_GHC_OPTS) Tests -o TestsP -main-is Tests.main

BenchmarkP : prof
	$(GHC_BIN) $(PROFILING_OPTS) -w TrieBench -o BenchmarkP -main-is TrieBench.main

Benchmark : opt
	$(GHC_BIN) $(OPTIMIZED_GHC_OPTS) -w TrieBench -o Benchmark -main-is TrieBench.main

TrieBench : TrieBench.hs opt
	$(GHC_BIN) $(OPTIMIZED_GHC_OPTS) -w $< -o $@ -main-is TrieBench.main
SetBench : SetBench.hs
	$(GHC_BIN) $(OPTIMIZED_GHC_OPTS) -w $< -o $@ -main-is SetBench.main
BSTrieBench : BSTrieBench.hs
	$(GHC_BIN) $(OPTIMIZED_GHC_OPTS) -w $< -o $@ -main-is BSTrieBench.main

Benchlog : opt
	$(GHC_BIN) $(THREADSCOPE_OPTS) -w TrieBench -o Benchlog -main-is TrieBench.main

clean:
	rm -f *.imports
	rm -rf out/
	rm -f SetBench TrieBench BenchmarkP Tests
	rm -f bench-SetBench.csv bench-TrieBench.csv bench.csv bench.png
	rm -f *.o *.hi