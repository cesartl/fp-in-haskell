{-# LANGUAGE Rank2Types #-}
import Data.Functor.Identity

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

modify :: Lens s a -> (a -> a) -> s -> s
modify lens g = runIdentity . lens h where
  h a = Identity $ g a

-- Lens s a = Functor f => (a -> f a) -> s -> f s
-- Lens a b = Functor f => (b -> f b) -> a -> f a
-- Lens s b = Functor f => (b -> f b) -> s -> f s
composeLens :: Lens s a  -> Lens a b -> Lens s b
composeLens = (.)

mkLens :: (s -> a) -> (s -> a -> s) -> Lens s a
-- afa :: a -> f a
mkLens get set afa s = fmap (set s) $ afa (get s)

get :: Lens s a -> s -> a


set :: Lens s a -> a -> s -> s