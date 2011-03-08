{-# LANGUAGE CPP, BangPatterns, ViewPatterns, FlexibleInstances #-}
module Data.TrieMap.RadixTrie.Alternate () where

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

{-# INLINE extractEdgeLoc #-}
extractEdgeLoc :: (Label v k, MonadPlus m) => Edge v k a -> Path v k a -> m (a, EdgeLoc v k a)
extractEdgeLoc EDGE(_ ks v ts) path = case v of
	Nothing	-> extractTS
	Just a	-> return (a, loc ks ts path) `mplus` extractTS
  where	extractTS = do	(e', tHole) <- alternate ts
			extractEdgeLoc e' (deep path ks v tHole)