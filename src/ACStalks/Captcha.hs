{-# LANGUAGE OverloadedStrings #-}

module ACStalks.Captcha (
    validateCaptcha
) where

import Control.Monad.IO.Class
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import Network.HTTP.Req

secret :: T.Text
secret = "6LchuOUUAAAAAAdkNF-ZywPcmR_lAWYtKfXM3Ggw"

validateCaptcha :: T.Text -> IO (Bool)
validateCaptcha response = runReq defaultHttpConfig $
    do
        let payload = "secret"   =: secret <>
                      "response" =: response 

        r <- req POST -- method
                (https "www.google.com" /: "recaptcha" /: "api" /: "siteverify") -- safe by construction URL
                (ReqBodyUrlEnc payload) -- use built-in options or add your own
                lbsResponse -- specify how to interpret response
                mempty       -- query params, headers, explicit port number, etc.

        let response = B.unpack $ responseBody r

        return (("true" :: String) `isInfixOf` response)
