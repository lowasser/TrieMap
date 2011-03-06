{-# LANGUAGE CPP, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, BangPatterns, RecordWildCards, TypeFamilies #-}
module Data.TrieMap.RadixTrie.Build () where

import Data.TrieMap.RadixTrie.Base
import Data.TrieMap.RadixTrie.Search

import Data.Functor.Immoral

import GHC.Exts

#define V(f) f (VVector) (k)
#define U(f) f (PVector) (Word)

instance TrieKey k => Buildable (TrieMap (VVector k)) (VVector k) where
  type UStack (TrieMap (VVector k)) = V(Edge)
  {-# INLINE uFold #-}
  uFold f = Foldl{
    zero = empty,
    begin = singletonEdge,
    snoc = \ e ks a -> insertEdge (f a) ks a e,
    done = Radix . Just}
  type AStack (TrieMap (VVector k)) = V(Stack)
  {-# INLINE aFold #-}
  aFold f = castMap $ fromAscListEdge f
  type DAStack (TrieMap (VVector k)) = V(Stack)
  {-# INLINE daFold #-}
  daFold = aFold const

instance Buildable (TrieMap (PVector Word)) (PVector Word) where
  type UStack (TrieMap (PVector Word)) = U(Edge)
  {-# INLINE uFold #-}
  uFold f = Foldl{
    zero = empty,
    begin = singletonEdge,
    snoc = \ e ks a -> insertEdge (f a) ks a e,
    done = WRadix . Just}
  type AStack (TrieMap (PVector Word)) = U(Stack)
  {-# INLINE aFold #-}
  aFold f = castMap $ fromAscListEdge f
  type DAStack (TrieMap (PVector Word)) = U(Stack)
  {-# INLINE daFold #-}
  daFold = aFold const

{-# SPECIALIZE fromAscListEdge ::
      (TrieKey k, Sized a) => (a -> a -> a) -> Foldl (V(Stack)) (V()) a (V(MEdge) a),
      Sized a => (a -> a -> a) -> Foldl (U(Stack)) (U()) a (U(MEdge) a) #-}
fromAscListEdge :: (Label v k, Sized a) => (a -> a -> a) -> 
  Foldl (Stack v k) (v k) a (MEdge v k a)
fromAscListEdge f = case inline daFold of
  Foldl{snoc = snocB, begin = beginB, done = doneB} 
    -> Foldl{..} where
    begin ks a = stack ks (Just a) Nothing Nothing
    zero = Nothing
    
    snoc stk ks vK = snoc' ks stk where
      snoc' !ks !stk = case sView stk of
	Stack ls !vL !brL !lStack -> iMatchSlice matcher matches ks ls where
	  matcher i k l z
	    | k == l	= z
	    | otherwise	= let
		ksPre = takeSlice i ks
		ksSuf = dropSlice (i+1) ks
		ls' = dropSlice (i+1) ls
		eL = roll (stack ls' vL brL lStack)
		in stack ksPre Nothing (Just (beginB l eL)) (Just (k, begin ksSuf vK))
	  matches kLen lLen
	    | kLen > lLen	= let
		ksPre = takeSlice lLen ks
		k = ks !$ lLen
		ksSuf = dropSlice (lLen + 1) ks
		in case lStack of
		  Just (lChar, lStack)
		    | k == lChar	-> stack ksPre vL brL (Just (lChar, snoc' ksSuf lStack))
		    | otherwise	-> stack ksPre vL (Just $ snocBranch brL lChar lStack)
					(Just (k, begin ksSuf vK))
		  Nothing	-> stack ksPre vL brL (Just (k, begin ksSuf vK))
	    | otherwise	= stack ks (Just (maybe vK (f vK) vL)) brL lStack
		
    
    snocBranch Nothing k stack = beginB k (roll stack)
    snocBranch (Just s) k stack = snocB s k (roll stack)
    
    roll stack = case sView stack of
      Stack ks (Just vK) _ Nothing	-> singletonEdge ks vK
      Stack ks vK brK (Just (kChar, stack')) ->
	edge ks vK $ inline doneB $ snocBranch brK kChar stack'
      _ -> error "Error: bad stack"
    
    done = Just . roll