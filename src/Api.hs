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
            :<|> "users" :> Get '[JSON] [Entity User]
            :<|> "users" :> Capture "userId" (Key User) :> Get '[JSON] (Entity User)
            :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] (Entity User)

server :: Server ApiDef
server = liftIO selectUsers
         :<|> liftIO selectUsers
         :<|> (\userId -> do
                  mUser <- liftIO $ getUser userId
                  case mUser of
                      Nothing -> throwError err404
                      Just u  -> pure u
              )
         :<|> (\user -> do
                  mRes <- liftIO $ insertUser user
                  case mRes of
                      Nothing -> throwError $ ServantErr {
                                     errHTTPCode = 204
                                   , errReasonPhrase = ""
                                   , errBody = ""
                                   , errHeaders = []
                                              }
                      Just u  -> pure u
              )

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
