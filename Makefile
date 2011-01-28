
.PHONY : opt bench clean

FAST_DIR := out/fast
OPTIMIZED_DIR := out/opt
FAST_GHC_OPTS := -O0 -odir $(FAST_DIR)
OPTIMIZED_GHC_OPTS := -O2 -fno-spec-constr-count -fno-spec-constr-threshold -odir $(OPTIMIZED_DIR)

fast : $(FAST_DIR)/Data/TrieSet.o
opt : $(OPTIMIZED_DIR)/Data/TrieSet.o

test : Tests
	./Tests

bench : Benchmark
	./Benchmark -s 30

Tests : fast
	ghc $(FAST_GHC_OPTS) Tests -o Tests -main-is Tests.main

Benchmark : opt
	ghc $(OPTIMIZED_GHC_OPTS) Benchmark -o Benchmark -main-is Benchmark.main

clean:
	rm -rf out/
	rm -f Benchmark
	rm -f Tests

# DO NOT DELETE: Beginning of Haskell dependencies

$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
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
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/TH.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Basic.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Prim.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/ByteString.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/Vectors.o
$(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances/ByteString.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
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
$(OPTIMIZED_DIR)/Data/TrieMap/IntMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/IntMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.o
$(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/OrdMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.o : $(OPTIMIZED_DIR)/Data/TrieMap/Modifiers.o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap/Key.o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Slice.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(OPTIMIZED_DIR)/Data/TrieMap/IntMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
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
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/IntMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/RadixTrie.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/ReverseMap.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Utils.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Sized.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation/Instances.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Representation.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/TrieKey.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Class/Instances.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Data/TrieMap/Class.o
$(OPTIMIZED_DIR)/Data/TrieMap.o : $(OPTIMIZED_DIR)/Control/Monad/Ends.o
$(OPTIMIZED_DIR)/Data/TrieSet.o : $(OPTIMIZED_DIR)/Data/TrieMap.o

$(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o : $(FAST_DIR)/Data/TrieMap/Representation/TH/Utils.o
$(FAST_DIR)/Data/TrieMap/Representation/TH/ReprMonad.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Modifiers.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
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
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.o : $(FAST_DIR)/Data/TrieMap/Representation/TH.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(FAST_DIR)/Data/TrieMap/Representation/TH.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Basic.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Foreign.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Prim.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.o : $(FAST_DIR)/Data/TrieMap/Utils.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/ByteString.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances/Vectors.o
$(FAST_DIR)/Data/TrieMap/Representation/Instances/ByteString.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
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
$(FAST_DIR)/Data/TrieMap/IntMap.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/IntMap.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap/OrdMap.o : $(FAST_DIR)/Data/TrieMap/Modifiers.o
$(FAST_DIR)/Data/TrieMap/OrdMap.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/OrdMap.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap/Key.o : $(FAST_DIR)/Data/TrieMap/Modifiers.o
$(FAST_DIR)/Data/TrieMap/Key.o : $(FAST_DIR)/Data/TrieMap/Representation/Class.o
$(FAST_DIR)/Data/TrieMap/Key.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/Key.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap/Key.o : $(FAST_DIR)/Data/TrieMap/Class.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(FAST_DIR)/Data/TrieMap/RadixTrie/Slice.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(FAST_DIR)/Data/TrieMap/IntMap.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap/RadixTrie/Edge.o : $(FAST_DIR)/Data/TrieMap/Sized.o
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
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/IntMap.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/RadixTrie.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/ReverseMap.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap/Class/Instances.o : $(FAST_DIR)/Data/TrieMap/Class.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Utils.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Sized.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Representation/Instances.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Representation.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/TrieKey.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Class/Instances.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Data/TrieMap/Class.o
$(FAST_DIR)/Data/TrieMap.o : $(FAST_DIR)/Control/Monad/Ends.o
$(FAST_DIR)/Data/TrieSet.o : $(FAST_DIR)/Data/TrieMap.o
# DO NOT DELETE: End of Haskell dependencies

$(OPTIMIZED_DIR)/%.o : %.hs
	ghc -c $(OPTIMIZED_GHC_OPTS) $<
$(FAST_DIR)/%.o : %.hs
	ghc -c $(FAST_GHC_OPTS) $<