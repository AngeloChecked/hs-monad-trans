{-# LANGUAGE OverloadedStrings #-}
module ScottyTypes where

import Web.Scotty
import Data.Text 

-- newtype Scotty e m a = 
--     ScottyT
--         { runS :: State (ScottyState e m) a }
--         deriving (Functor, Applicative, Monad)

-- newtype ActionT e m a =
--     ActionT { runAM
--         :: ExceptT
--             (ActionError e)
--             (ReaderT ActionEnv
--                 (StateT ScottyRespone m))
--             a
--         }
--     deriving (Funtor, Applicative)

-- type ScottyM = Scotty Text IO
-- type ActionM = ActionT Text IO


