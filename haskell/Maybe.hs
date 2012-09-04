
minus5 Nothing = Nothing
minus5 (Just x) 
    | x == 7    = Nothing
    | otherwise = Just (x - 5)

mal3 Nothing = Nothing
mal3 (Just x) 
    | even x    = Nothing
    | otherwise = Just (3 * x)

plus7 Nothing = Nothing
plus7 (Just x) = Just (x + 7)

Nothing  >>- f = Nothing
(Just x) >>- f = f x


retrn x = Just x

get (Just x) = x

_minus5 x
    | x == 7    = Nothing
    | otherwise = Just (x - 5)

_mal3 x
    | even x    = Nothing
    | otherwise = Just (3 * x)

_plus7 x = Just (x + 7)


-- (Just 13) >>= _plus7 >>= _mal3 >>= _minus5

x >>>= f = f x
ret x = x

data Throws a b = ReturnValue a
                | Exception b

return x = ReturnValue x

(>>>-) :: Throws a b -> (a -> Throws c b) -> Throws c b
ReturnValue x >>>- f = f x
Exception x   >>>- f = Exception x


main = do {
     print (minus5 (mal3 (plus7 (Just 12))));
     print (retrn 12 >>- _plus7 >>- _mal3 >>- _minus5);
     print (Nothing >>- _plus7 >>- _mal3 >>- _minus5)
}

