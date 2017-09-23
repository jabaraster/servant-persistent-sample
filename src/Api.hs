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
         -- :<|> liftIO . getUser >>= toResponse'
         :<|> (\userId -> do
                  mUser <- liftIO $ getUser userId
                  toResponse' err404 mUser
              )
         :<|> (\user -> do
                  mRes <- liftIO $ insertUser user
                  toResponse 204 mRes
              )

toResponse :: Int -> Maybe a -> Handler a
toResponse responseCode Nothing = throwError $ ServantErr {
                errHTTPCode = responseCode
              , errReasonPhrase = ""
              , errBody = ""
              , errHeaders = []
            }
toResponse _ (Just a) = pure a

toResponse' :: ServantErr -> Maybe a -> Handler a
toResponse' err Nothing = throwError err
toResponse' _ (Just a)  = pure a

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
