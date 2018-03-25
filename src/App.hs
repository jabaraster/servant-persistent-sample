{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module App where

import           Control.Monad.IO.Class    (liftIO)
import           Database.Persist.Sql
import           DataStore
import           DataStore.Internal        (pgPool)
import           Network.HTTP.Types.Status
import           Network.Wai.Handler.Warp  (Port, run)
import           Servant
import           Servant.API

type ApiDef  = Get '[JSON] [Entity User]
            :<|> "users" :> Get '[JSON] [Entity User]
            :<|> "users" :> Capture "userId" (Key User) :> Get '[JSON] (Entity User)
            :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] (Entity User)

server :: ConnectionPool -> Server ApiDef
server pool = (liftIO $ getUsers pool)
         :<|> (liftIO $ getUsers pool)
         :<|> (\userId -> do
                  mUser <- liftIO $ getUser pool userId
                  toResponse' err404 mUser
              )
         :<|> (\user -> do
                  mRes <- liftIO $ insertUser pool user
                  toResponse status204 mRes
              )

toResponse :: Status -> Maybe a -> Handler a
toResponse responseCode Nothing = throwError $ ServantErr {
                errHTTPCode = statusCode responseCode
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

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: IO Application
mkApp = do
  migrateDb
  pool <- pgPool
  return $ app pool

startServer :: Port -> IO ()
startServer port = do
    putStrLn "{- ----------------------------"
    putStrLn " - start server!"
    putStrLn " ----------------------------- -}"
    run port =<< mkApp
