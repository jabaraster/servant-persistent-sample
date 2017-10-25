module App where

import DataStore.Internal

type ApiDef  = Get '[JSON] [Entity User]
            :<|> "users" :> Get '[JSON] [Entity User]
            :<|> "users" :> Capture "userId" (Key User) :> Get '[JSON] (Entity User)
            :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] (Entity User)

server :: ConnectinoPool -> Server ApiDef
server pool = liftIO selectUsers
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


app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: IO Application
mkApp = do
  pool <- pgPool
  runSqlPool (doMigration migrateAll) pool
  return $ app pool
