data Lens s a  = Lens { get :: s -> a, set :: a -> s -> s }

modify :: Lens s a -> (a -> a) -> s -> s
modify lens f s = set lens (f (get lens s)) s

composeLens :: Lens s a  -> Lens a b -> Lens s b
composeLens sa sb = Lens newGet newSet where
  newGet = (get sb) . (get sa)
  -- (set sa) :: a -> s -> s
  -- (get sa) :: s -> a
  -- (set sb) :: b -> a -> a
  -- (get sb) :: s -> b
  -- newSet   :: b -> s -> s
  -- newSet b s = (set sa) ((set sb) b (get sa s)) s
  newSet = modify sa . (set sb)