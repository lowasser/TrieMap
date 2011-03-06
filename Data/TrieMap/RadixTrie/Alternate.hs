{-# LANGUAGE CPP, BangPatterns, ViewPatterns, FlexibleInstances #-}
module Data.TrieMap.RadixTrie.Alternate () where

import Control.Monad.Ends

import Data.TrieMap.RadixTrie.Base

#define V(f) f (VVector) (k)
#define U(f) f (PVector) (Word)
#define EDGE(args) (!(eView -> Edge args))

instance TrieKey k => Alternatable (TrieMap (VVector k)) where
  alternate (Radix Nothing) = mzero
  alternate (Radix (Just m)) = do 
    (a, loc) <- extractEdgeLoc m root
    return (a, Hole loc)

instance Alternatable (TrieMap (PVector Word)) where
  alternate (WRadix Nothing) = mzero
  alternate (WRadix (Just m)) = do 
    (a, loc) <- extractEdgeLoc m root
    return (a, WHole loc)

{-# SPECIALIZE extractEdgeLoc :: 
      TrieKey k => V(Edge) a -> V(Path) a -> First (a, V(EdgeLoc) a),
      TrieKey k => V(Edge) a -> V(Path) a -> Last (a, V(EdgeLoc) a),
      U(Edge) a -> U(Path) a -> First (a, U(EdgeLoc) a),
      U(Edge) a -> U(Path) a -> Last (a, U(EdgeLoc) a) #-}
extractEdgeLoc :: (Label v k, MonadPlus m) => Edge v k a -> Path v k a -> m (a, EdgeLoc v k a)
extractEdgeLoc EDGE(_ ks v ts) path = case v of
	Nothing	-> extractTS
	Just a	-> return (a, loc ks ts path) `mplus` extractTS
  where	extractTS = do	(e', tHole) <- alternate ts
			extractEdgeLoc e' (deep path ks v tHole)