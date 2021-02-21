module Morra where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char

data Choise = Evens
            | Odds 
            deriving (Show)

validateChoise :: String -> Maybe Choise
validateChoise s 
            | lowerS == (toLower <$> show Evens) = Just Evens
            | lowerS == (toLower <$> show Odds) = Just Odds 
            | otherwise = Nothing 
            where lowerS = toLower <$> s


main = do
    print "choose odds or evens:"
    x  <- getLine 
    
    print $ validateChoise x
    main