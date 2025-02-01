{-# LANGUAGE FlexibleInstances #-}

instance Monoid Bool where
  mempty = False
  mappend = (||)

instance Semigroup Bool where
  (<>) = (||)

-- or:
-- instance Monoid Bool where
--   mempty = True
--   mappend = (&&)
--
-- instance Semigroup Bool where
--   (<>) = (&&)

instance Monoid (a -> a) where
  mempty = id
  mappend = (.)

instance Semigroup (a -> a) where
  (<>) = (.)
