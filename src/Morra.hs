module Morra where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char
import Data.Maybe
import System.Random
import Text.Read (readMaybe)

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
    choise <- getLine 
    case validateChoise choise of 
        Nothing -> do 
            print "sbagliato, riprova"
            main
        Just x -> do
            print $ "hai scelto " ++ show x 
            let playerInput = do
                print "segli il tuo numero:"
                mNumber <- getLine
                case readMaybe mNumber of
                    Nothing -> do
                        print "non hai scritto un numero, riprova"
                        playerInput 
                    Just a -> pure a
            number <- playerInput 
            print $ "P: " ++ show number
            r <- randomRIO (0, 9) :: IO Int
            print $ "C: " ++ show r
            print (r + number)
       
