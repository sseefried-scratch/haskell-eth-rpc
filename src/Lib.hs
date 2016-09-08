{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Lib
    ( ethAccounts
    ) where


import Control.Lens
import Control.Monad.Except
import Data.Aeson
import Data.Text
import Network.JsonRpc.Client
import Network.Wreq

eth_accounts :: Signature () [Text]
eth_accounts = Signature "eth_accounts" ()

conn :: Connection IO
conn bs = do
  res <- post "http://localhost:8545" bs
  return $ Just $ res ^. responseBody

type RpcIOResult a = IO (Either RpcError a)

lift0 :: ( ComposeMultiParam (Batch r -> RpcResult IO r) f (RpcResult IO r)
       , ClientFunction ps r f)
    => Signature ps r -> IO (Either RpcError r)
lift0 sig = runExceptT $ toFunction conn sig

lift1 :: ( ComposeMultiParam (Batch r -> RpcResult IO r) f (a -> RpcResult IO r)
        , ClientFunction ps r f)
     => Signature ps r -> a -> IO (Either RpcError r)
lift1 sig = \a -> (runExceptT . toFunction conn sig) a

lift2 :: ( ComposeMultiParam (Batch r -> RpcResult IO r) f (a -> b -> RpcResult IO r)
        , ClientFunction ps r f)
     => Signature ps r -> a -> b -> IO (Either RpcError r)
lift2 sig = \a b -> runExceptT $ toFunction conn sig a b




ethAccounts :: IO (Either RpcError [Text])
ethAccounts = lift0 eth_accounts

netListening = lift0 (Signature "net_listening" () :: Signature () Bool)

eth_getCode :: Signature (Text ::: ()) Text
eth_getCode = Signature "eth_getCode" ("addr" ::: ())

ethGetCode :: Text -> IO (Either RpcError Text)
ethGetCode = lift1 eth_getCode