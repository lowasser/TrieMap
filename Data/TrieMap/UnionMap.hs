{-# LANGUAGE UnboxedTuples, TypeFamilies, PatternGuards, ViewPatterns, CPP, BangPatterns, FlexibleInstances, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, MagicHash #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.TrieMap.UnionMap () where

import Data.TrieMap.TrieKey
import Data.TrieMap.UnitMap ()

import GHC.Exts

import Prelude hiding (foldr, foldr1, foldl, foldl1, (^))

{-# INLINE (^) #-}
(^) :: (TrieKey k1, TrieKey k2, Sized a) => Maybe (TrieMap k1 a) -> Maybe (TrieMap k2 a) -> Maybe (TrieMap (Either k1 k2) a)
Nothing ^ Nothing	= Nothing
Just m1 ^ Nothing	= Just $ MapL m1
Nothing ^ Just m2	= Just $ MapR m2
Just m1 ^ Just m2	= Just $ Union (sizeM m1 + sizeM m2) m1 m2

(^>) :: (TrieKey k1, TrieKey k2, Sized a) => Maybe (TrieMap k1 a) -> TrieMap k2 a -> TrieMap (Either k1 k2) a
Nothing ^> mR = MapR mR
Just mL ^> mR = mapLR mL mR

(<^) :: (TrieKey k1, TrieKey k2, Sized a) => TrieMap k1 a -> Maybe (TrieMap k2 a) -> TrieMap (Either k1 k2) a
mL <^ Nothing = MapL mL
mL <^ Just mR = mapLR mL mR

mapLR :: (TrieKey k1, TrieKey k2, Sized a) => TrieMap k1 a -> TrieMap k2 a -> TrieMap (Either k1 k2) a
mapLR m1 m2 = Union (sizeM m1 + getSize m2) m1 m2

singletonL :: (TrieKey k1, TrieKey k2, Sized a) => k1 -> a -> TrieMap (Either k1 k2) a
singletonL k a = MapL (singletonM k a)

singletonR :: (TrieKey k1, TrieKey k2, Sized a) => k2 -> a -> TrieMap (Either k1 k2) a
singletonR k a = MapR (singletonM k a)

data UView k1 k2 a = UView (Maybe (TrieMap k1 a)) (Maybe (TrieMap k2 a))
data HView k1 k2 a = Hole1 (Hole k1 a) (Maybe (TrieMap k2 a))
		    | Hole2 (Maybe (TrieMap k1 a)) (Hole k2 a)		    

{-# INLINE uView #-}
uView :: TrieMap (Either k1 k2) a -> UView k1 k2 a
uView (MapL m1) = UView (Just m1) Nothing
uView (MapR m2) = UView Nothing (Just m2)
uView (Union _ m1 m2) = UView (Just m1) (Just m2)

hView :: Hole (Either k1 k2) a -> HView k1 k2 a
hView (HoleX0 hole1) = Hole1 hole1 Nothing
hView (HoleXR hole1 m2) = Hole1 hole1 (Just m2)
hView (Hole0X hole2) = Hole2 Nothing hole2
hView (HoleLX m1 hole2) = Hole2 (Just m1) hole2

hole1 :: Hole k1 a -> Maybe (TrieMap k2 a) -> Hole (Either k1 k2) a
hole1 hole1 Nothing = HoleX0 hole1
hole1 hole1 (Just m2) = HoleXR hole1 m2

hole2 :: Maybe (TrieMap k1 a) -> Hole k2 a -> Hole (Either k1 k2) a
hole2 Nothing hole2 = Hole0X hole2
hole2 (Just m1) hole2 = HoleLX m1 hole2

#define UVIEW uView -> UView
#define CONTEXT(cl) (TrieKey k1, TrieKey k2, cl (TrieMap k1), cl (TrieMap k2))

instance CONTEXT(Functor) => Functor (TrieMap (Either k1 k2)) where
  fmap f (MapL m1) = MapL (f <$> m1)
  fmap f (MapR m2) = MapR (f <$> m2)
  fmap f (Union s m1 m2) = Union s (f <$> m1) (f <$> m2)

instance CONTEXT(Foldable) => Foldable (TrieMap (Either k1 k2)) where
  foldMap f (UVIEW m1 m2) = fmap (foldMap f) m1 `mappendM` fmap (foldMap f) m2
  foldr f z (UVIEW m1 m2) = foldl (foldr f) (foldl (foldr f) z m2) m1
  foldl f z (UVIEW m1 m2) = foldl (foldl f) (foldl (foldl f) z m1) m2

instance CONTEXT(Traversable) => Traversable (TrieMap (Either k1 k2)) where
  traverse f (MapL m1) = MapL <$> traverse f m1
  traverse f (MapR m2) = MapR <$> traverse f m2
  traverse f (Union s m1 m2) = Union s <$> traverse f m1 <*> traverse f m2

instance CONTEXT(Subset) => Subset (TrieMap (Either k1 k2)) where
  (UVIEW m11 m12) <=? (UVIEW m21 m22)
    = m11 <<=? m21 && m12 <<=? m22

instance (TrieKey k1, TrieKey k2) => Buildable (TrieMap (Either k1 k2)) (Either k1 k2) where
  type UStack (TrieMap (Either k1 k2)) = TrieMap (Either k1 k2)
  uFold = defaultUFold singletonM insertWithM
  type AStack (TrieMap (Either k1 k2)) = Stack (AMStack k1) (AMStack k2)
  aFold f = unionFold (aFold f) (aFold f)
  type DAStack (TrieMap (Either k1 k2)) = Stack (DAMStack k1) (DAMStack k2)
  daFold = unionFold daFold daFold

{-# INLINE runUView #-}
runUView :: TrieMap (Either k1 k2) a -> (Maybe (TrieMap k1 a) -> Maybe (TrieMap k2 a) -> r) -> r
runUView (MapL mL) f = inline f (Just mL) Nothing
runUView (MapR mR) f = inline f Nothing (Just mR)
runUView (Union _ mL mR) f = inline f (Just mL) (Just mR)

instance CONTEXT(SetOp) => SetOp (TrieMap (Either k1 k2)) where
  union f m1 m2 = runUView m1 (runUView m2 .: run)
    where {-# INLINE run #-}
	  run m1L m1R m2L m2R 
	    = unionM (union f) m1L m2L ^ unionM (union f) m1R m2R
  isect f m1 m2 = runUView m1 (runUView m2 .: run) where
    run m1L m1R m2L m2R = isectM (isect f) m1L m2L ^ isectM (isect f) m1R m2R
  diff f m1 m2 = runUView m2 (runUView m1 .: run) where
    run m2L m2R m1L m1R = diffM (diff f) m1L m2L ^ diffM (diff f) m1R m2R

instance CONTEXT(Project) => Project (TrieMap (Either k1 k2)) where
  mapMaybe f (UVIEW m1 m2) = mapMaybeM (mapMaybe f) m1 ^ mapMaybeM (mapMaybe f) m2
  mapEither f (UVIEW m1 m2) = (# m11 ^ m21, m12 ^ m22 #)
    where !(# m11, m12 #) = mapEitherM (mapEither f) m1
	  !(# m21, m22 #) = mapEitherM (mapEither f) m2

-- | @'TrieMap' ('Either' k1 k2) a@ is essentially a @(TrieMap k1 a, TrieMap k2 a)@, but
-- specialized for the cases where one or both maps are empty.
instance (TrieKey k1, TrieKey k2) => TrieKey (Either k1 k2) where
	{-# SPECIALIZE instance TrieKey (Either () ()) #-}  
	data TrieMap (Either k1 k2) a = 
		MapL (TrieMap k1 a)
		| MapR (TrieMap k2 a)
		| Union !Int (TrieMap k1 a) (TrieMap k2 a)
	data Hole (Either k1 k2) a =
		HoleX0 (Hole k1 a)
		| HoleXR (Hole k1 a) (TrieMap k2 a)
		| Hole0X (Hole k2 a)
		| HoleLX (TrieMap k1 a) (Hole k2 a)
	
	singletonM = either singletonL singletonR
	
	getSimpleM (MapL m1) = getSimpleM m1
	getSimpleM (MapR m2) = getSimpleM m2
	getSimpleM Union{} = NonSimple
	
	sizeM (MapL m1) = sizeM m1
	sizeM (MapR m2) = sizeM m2
	sizeM (Union s _ _) = s
	
	lookupMC (Left k) (UVIEW (Just m1) _) = lookupMC k m1
	lookupMC (Right k) (UVIEW _ (Just m2)) = lookupMC k m2
	lookupMC _ _ = mzero

	insertWithM f k a m = case (k, m) of
	  (Left k, MapL mL) -> MapL (insertWithM f k a mL)
	  (Right k, MapR mR) -> MapR (insertWithM f k a mR)
	  (Left k, MapR mR) -> mapLR (singletonM k a) mR
	  (Right k, MapL mL) -> mapLR mL (singletonM k a)
	  (Left k, Union _ mL mR) -> mapLR (insertWithM f k a mL) mR
	  (Right k, Union _ mL mR) -> mapLR mL (insertWithM f k a mR)

	singleHoleM = either (HoleX0 . singleHoleM) (Hole0X . singleHoleM)

	beforeM hole = case hView hole of
		Hole1 h1 __	-> beforeM h1 ^ Nothing
		Hole2 m1 h2	-> m1 ^ beforeM h2
	beforeWithM a hole = case hView hole of
		Hole1 h1 __	-> MapL (beforeWithM a h1)
		Hole2 m1 h2	-> maybe MapR mapLR m1 (beforeWithM a h2)
	
	afterM hole = case hView hole of
		Hole1 h1 m2	-> afterM h1 ^ m2
		Hole2 __ h2	-> Nothing ^ afterM h2
	afterWithM a hole = case hView hole of
		Hole1 h1 m2	-> afterWithM a h1 <^ m2
		Hole2 __ h2	-> MapR (afterWithM a h2)
	
	searchMC (Left k) (UVIEW m1 m2) = mapSearch (`hole1` m2) (searchMC' k m1)
	searchMC (Right k) (UVIEW m1 m2) = mapSearch (hole2 m1) (searchMC' k m2)
	
	indexM m i = case m of
	  MapL m1	-> case indexM m1 i of
	    (# i', a, hole1 #) -> (# i', a, HoleX0 hole1 #)
	  MapR m2	-> case indexM m2 i of
	    (# i', a, hole2 #) -> (# i', a, Hole0X hole2 #)
	  Union _  m1 m2
	    | i <# s1, (# i', a, hole1 #) <- indexM m1 i
	    	-> (# i', a, HoleXR hole1 m2 #)
	    | (# i', a, hole2 #) <- indexM m2 (i -# s1)
		-> (# i', a, HoleLX m1 hole2 #)
	    where !s1 = sizeM# m1

	extractHoleM (UVIEW !m1 !m2) = holes1 `mplus` holes2 where
	  holes1 = holes extractHoleM (`hole1` m2) m1
	  holes2 = holes extractHoleM (hole2 m1) m2
	
	clearM hole = case hView hole of
		Hole1 h1 m2	-> clearM h1 ^ m2
		Hole2 m1 h2	-> m1 ^ clearM h2
	assignM v hole = case hView hole of
		Hole1 h1 m2	-> assignM v h1 <^ m2
		Hole2 m1 h2	-> m1 ^> assignM v h2
	
	unifierM (Left k') (Left k) a = HoleX0 <$> unifierM k' k a
	unifierM (Left k') (Right k) a = return $ HoleXR (singleHoleM k') (singletonM k a)
	unifierM (Right k') (Left k) a = return $ HoleLX (singletonM k a) (singleHoleM k')
	unifierM (Right k') (Right k) a = Hole0X <$> unifierM k' k a
	
	unifyM (Left k1) a1 (Left k2) a2 = MapL <$> unifyM k1 a1 k2 a2
	unifyM (Left k1) a1 (Right k2) a2 = return $ singletonM k1 a1 `mapLR` singletonM k2 a2
	unifyM (Right k2) a2 (Left k1) a1 = return $ singletonM k1 a1 `mapLR` singletonM k2 a2
	unifyM (Right k1) a1 (Right k2) a2 = MapR <$> unifyM k1 a1 k2 a2

{-# INLINE holes #-}
holes :: (Functor m, Functor f, MonadPlus m) => (a -> m (f b)) -> (b -> c) -> Maybe a -> m (f c)
holes k f (Just a) = fmap f <$> k a
holes _ _ Nothing = mzero

{-# INLINE unionFold #-}
unionFold :: (TrieKey k1, TrieKey k2, Sized a) =>
  FromList z1 k1 a -> FromList z2 k2 a -> FromList (Stack z1 z2) (Either k1 k2) a
unionFold Foldl{snoc = snocL, begin = beginL, done = doneL}
	    Foldl{snoc = snocR, begin = beginR, done = doneR}
  = Foldl{..}
  where	snoc (JustL s1)	(Left k) a = JustL (snocL s1 k a)
	snoc (JustL s1)	(Right k) a = Both s1 (beginR k a)
	snoc (JustR s2) (Left k) a = Both (beginL k a) s2
	snoc (JustR s2) (Right k) a = JustR (snocR s2 k a)
	snoc (Both s1 s2) (Left k) a = Both (snocL s1 k a) s2
	snoc (Both s1 s2) (Right k) a = Both s1 (snocR s2 k a)
	
	begin (Left k) a = JustL (beginL k a)
	begin (Right k) a = JustR (beginR k a)
	
	done (JustL sL) = MapL (doneL sL)
	done (JustR sR) = MapR (doneR sR)
	done (Both sL sR) = doneL sL `mapLR` doneR sR

data Stack s1 s2 a =
  JustL (s1 a)
  | JustR (s2 a)
  | Both (s1 a) (s2 a)