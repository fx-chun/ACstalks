module ACStalks.Schema.Token (
    Token(..)
) where

import qualified Data.Text as T

data Token = Token { token :: T.Text
                   , tokenUserId :: Int }
                   
