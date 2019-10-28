-- <*>  :: Applicative f => f (a -> b) -> f a -> f b 
-- <*> :: Applicative f => f (a -> (b ->c)  -> f a -> f (b -> c)
map2 :: Applicative f => (a -> b -> c) ->  f a -> f b -> f c
map2 g fa fb = g <$> fa <*> fb

-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse :: Traverse f => Applicative g => (a -> g b) -> f a -> g f b

foo :: (a -> b) -> a -> (b, b)
foo f a = (f a, f a)

bar :: Traversable f => Monoid b => (a -> b) -> f a -> (b, f b)
bar g = traverse (foo g)

data Foo a b = Foo { content :: a}

instance Functor (Foo a) where
  fmap _ (Foo a) = Foo a

instance Monoid a => Applicative (Foo a) where
  pure _ = Foo mempty
  (Foo a1) <*> (Foo a2) = Foo $ mappend a1 a2

foldMap :: Traversable f => Monoid b => (a -> b) -> f a -> b
-- foldMap g fa = fst (bar g fa)
foldMap g = content . traverse h where
  h a = Foo $ g a