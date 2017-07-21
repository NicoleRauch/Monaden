import Control.Applicative -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

data Exceptional e a =
     Success a
   | Exception e
   deriving (Show)

-- as per the Functor-Applicative-Monad Proposal: (https://wiki.haskell.org/Functor-Applicative-Monad_Proposal)
instance Functor (Exceptional e) where
  fmap = liftM

instance Applicative (Exceptional e) where
  pure  = return
  (<*>) = ap

instance Monad (Exceptional e) where
   return              =  Success
   Exception l >>= _   =  Exception l
   Success  r  >>= k   =  k r
 
throw :: e -> Exceptional e a
throw = Exception
 
catch :: Exceptional e a -> (e -> Exceptional e a) -> Exceptional e a
catch (Exception  l) h = h l
catch (Success r)    _ = Success r



data Exceptions = IllegalArgumentException String
  deriving (Show)

throwSomething x
   | x == 7    = throw (IllegalArgumentException "Ich hasse 7!")
   | otherwise = Success (x + 1)

mal3 :: Int -> Int
mal3 x = 3 * x

main = do {
   print $ (throwSomething 7) >>= (return . mal3);
   print $ (throwSomething 12) >>= (return . mal3)
}
