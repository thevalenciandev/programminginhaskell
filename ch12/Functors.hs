-- Can be used to increment any functorial type, not just lists
inc :: Functor f => f Int -> f Int
inc = fmap (+1)
