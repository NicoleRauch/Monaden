data Exceptional e a =
     Success a
   | Exception e
   deriving (Show)
 
instance Monad (Exceptional e) where
   return              =  Success
   Exception l >>= _   =  Exception l
   Success  r  >>= k   =  k r
 
throw :: e -> Exceptional e a
throw = Exception
 
catch :: Exceptional e a -> (e -> Exceptional e a) -> Exceptional e a
catch (Exception  l) h = h l
catch (Success r)    _ = Success r



data Exceptions = RuntimeException String

throwSomething x
   | x == 7    = throw (RuntimeException "Ich hasse 7!")
   | otherwise = Success (x + 1)

mal3 :: Int -> Int
mal3 x = 3 * x

resultOf (Exception (RuntimeException s)) = "Error: " ++ s
resultOf (Success r) = "Result: " ++ (show r)

main = do {
   putStrLn (resultOf $ (throwSomething 7) >>= (return . mal3));
   putStrLn (resultOf $ (throwSomething 12) >>= (return . mal3))
}
