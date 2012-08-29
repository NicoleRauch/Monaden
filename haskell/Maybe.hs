minus5 Nothing = Nothing
minus5 (Just x) = Just (x - 5)

mal3 Nothing = Nothing
mal3 (Just x) = Just (3 * x)

plus7 Nothing = Nothing
plus7 (Just x) = Just (x + 7)

(>>=) Nothing  f = Nothing
(>>=) (Just x) f = Just (f x)


return x = Just x

get (Just x) = x



_minus5 x = x - 5
_mal3   x = 3 * x
_plus7  x = x + 7

(Just 13) >>= _plus7 >>= _mal3 >>= _minus5


main = do
     putStrLn (show (minus5 (mal3 (plus7 (Just 13))))) >>
     putStrLn (Just 13) >>= _plus7 >>= _mal3 >>= _minus5

