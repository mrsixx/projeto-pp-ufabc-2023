{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where
  import Database.Persist.Sql
  import Database.Persist.TH
  import Data.Aeson
  import Control.Monad (mzero)
  import Data.Text (Text)

  data TipoOperacao = Deposito | Transferencia | Saque deriving (Show, Eq)

  instance ToJSON TipoOperacao where
      toJSON Deposito = String "1"
      toJSON Transferencia = String "2"
      toJSON Saque = String "3"

  instance FromJSON TipoOperacao where
      parseJSON (String "1") = pure Deposito
      parseJSON (String "2") = pure Transferencia
      parseJSON (String "3") = pure Saque
      parseJSON _ = fail "Invalid value"
  
  instance PersistField TipoOperacao where
    toPersistValue Deposito = PersistText "1"
    toPersistValue Transferencia = PersistText "2"
    toPersistValue Saque = PersistText "3"

    fromPersistValue (PersistText "1") = Right Deposito
    fromPersistValue (PersistText "2") = Right Transferencia
    fromPersistValue (PersistText "3") = Right Saque
    fromPersistValue _ = Left "Invalid value"

  instance PersistFieldSql TipoOperacao where
    sqlType _ = SqlString