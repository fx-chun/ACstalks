module ACStalks.API.RouteAdmin (
    AdminApi,
    adminServer, 
) where

import ACStalks.Database
import ACStalks.Schema
import Control.Monad
import Control.Monad.IO.Class
import Crypto.BCrypt
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Maybe
import GHC.Generics
import Servant
import Text.Read (readEither)

type AdminApi = "ban" :> ReqBody '[JSON] RequestUser :> Post '[PlainText] String              :<|> "clear"
                   :> ReqBody '[JSON] RequestUser
                   :> Post '[PlainText] String

data RequestUser = RequestUser { adminToken :: T.Text
                               , uid :: Int } 
    deriving (Generic)

instance FromJSON RequestUser

validateRequest :: DatabaseConnection -> T.Text -> IO (String) -> IO (String)
validateRequest dbc token valid = do
    result <- validateToken dbc token
    case result of
        Just user -> if (userId user == 1) then valid 
                     else return "authfail"
        Nothing   -> return "authfail"

adminServer :: DatabaseConnection -> Server AdminApi
adminServer dbc = banUser :<|> clearUserData
    where
        banUser :: RequestUser -> Handler String 
        banUser req = liftIO 
                                 $ validateRequest dbc (adminToken req)
                                 $ 
            do
                deleteUserByUid dbc (uid req)
                deletePriceByUid dbc (uid req)
                return "true"

        clearUserData :: RequestUser -> Handler String
        clearUserData req = liftIO 
                                       $ validateRequest dbc (adminToken req)
                                       $ 
            do
                user <- fmap fromJust $ getUser dbc (uid req)

                deletePriceByUid dbc (uid req)
                updateUser dbc $
                    userDefaults { userId = userId user
                                 , userName = userName user
                                 , userPasshash = userPasshash user }

                return "true"

