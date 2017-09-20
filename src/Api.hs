{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Api (
    main
    ) where

import           Control.Monad.IO.Class       (liftIO)
import           Data.Text                    (Text)
import           Database.Persist
import           DataStore
import           Network.Wai.Handler.Warp
import           Servant


type ApiDef  = Get '[JSON] [Entity User]
            :<|> "users" :> Capture "userId" (Key User) :> Get '[JSON] (Maybe (Entity User))
            :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] (Entity User)

server :: Server ApiDef
server = liftIO selectUsers
         :<|> (\userId -> liftIO $ getUser userId)
         :<|> liftIO . insertUser

api :: Proxy ApiDef
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
    putStrLn "{- ----------------------------"
    putStrLn " - start server!"
    putStrLn " ----------------------------- -}"
    migrate
    run 1234 app
