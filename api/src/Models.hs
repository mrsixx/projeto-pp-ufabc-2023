{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Models where

import           Web.Spock
import           Web.Spock.Config
import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           Data.Time        (UTCTime)
import           GHC.Generics
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT, logInfo)
import           Database.Persist        hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH
import           Types            (TipoOperacao)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Correntista json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
  nome Text
  cpf Text
  senha Text
  deriving Show

ContaCorrente json
  correntistaId CorrentistaId
  numConta Text
  deriving Show

OperacaoFinanceira json
  contaOrigemId ContaCorrenteId
  contaDestinoId ContaCorrenteId
  valor Double
  dataOperacao UTCTime
  tipo TipoOperacao
  deriving Show
|]