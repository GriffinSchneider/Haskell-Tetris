module ListComprehension (
    ormap,
    andmap,
) where

ormap :: (a -> Bool) -> [a] -> Bool
ormap f as = foldl (\ acc a -> acc || (f a)) False as

andmap :: (a -> Bool) -> [a] -> Bool
andmap f as = foldl (\ acc a -> acc && (f a)) True as
