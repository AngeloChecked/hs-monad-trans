module Fix where

import Control.Monad.Trans.Maybe

import Control.Monad.IO.Class
import Control.Monad

isValid :: String -> Bool 
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
    v <- liftIO getLine
    guard $ isValid v
    return v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "Moar Excite"
        Just e -> putStrLn ("Good, was very excite: " ++ e) 

