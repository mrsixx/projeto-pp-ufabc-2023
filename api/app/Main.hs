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
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Web.Spock
import           Web.Spock.Config
import           Network.Wai.Middleware.Cors
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
import           Lib                (calcularSaldo)
import           Models

type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy
    { corsOrigins = Nothing  -- Defina aqui as origens permitidas (Nothing para permitir qualquer origem)
    , corsMethods = ["GET", "POST"]
    , corsRequestHeaders = ["Content-Type"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

obterOperacoesConta contaIdKey = runSQL $ selectList
  [
    FilterOr[
      OperacaoFinanceiraContaOrigemId ==. contaIdKey,
      OperacaoFinanceiraContaDestinoId ==. contaIdKey
    ]
  ]
  [Desc OperacaoFinanceiraDataOperacao]

app :: Api
app = do
  -- Configuração do middleware CORS
  middleware $ cors $ const $ Just corsPolicy
  post "login" $ do
    maybeLogin <- jsonBody :: ApiAction (Maybe Login)
    case maybeLogin of
      Nothing -> errorJson 500 "Failed to parse request body as Login"
      Just (Login cpf senha) -> do
        maybeCorrentista <- runSQL $ selectFirst [CorrentistaCpf ==. cpf, CorrentistaSenha ==. senha] []
        case maybeCorrentista of
          Nothing -> errorJson 404 "Invalid Login."
          Just (Entity correntistaId correntista) -> do
              json $ object ["result" .= String "success", "id" .= correntistaId]
        
  get "correntista" $ do
    correntistas <- runSQL $ selectList [] [Asc CorrentistaId]
    json correntistas

  get ("correntista" <//> var) $ \correntistaId -> do
    maybeCorrentista <- runSQL $ P.get correntistaId :: ApiAction (Maybe Correntista)
    case maybeCorrentista of
      Nothing -> errorJson 404 "Could not find a Correntista entity with matching id."
      Just correntista -> json correntista

  post "correntista" $ do
    maybeCorrentista <- jsonBody :: ApiAction (Maybe Correntista)
    case maybeCorrentista of
      Nothing -> errorJson 500 "Failed to parse request body as Correntista"
      Just correntista -> do
        newId <- runSQL $ insert correntista
        json $ object ["result" .= String "success", "id" .= newId]
        
  -- Fim endpoints correntistas

-- Endpoints conta
  get "conta-corrente" $ do
    contas <- runSQL $ selectList [] [Asc ContaCorrenteId]
    json contas

  get ("conta-corrente" <//> var) $ \contaId -> do
    maybeConta <- runSQL $ P.get contaId :: ApiAction (Maybe ContaCorrente)
    case maybeConta of
      Nothing -> errorJson 404 "Could not find a ContaCorrente entity with matching id."
      Just conta -> json conta

  post "conta-corrente" $ do
    maybeConta <- jsonBody :: ApiAction (Maybe ContaCorrente)
    case maybeConta of
      Nothing -> errorJson 500 "Failed to parse request body as ContaCorrente"
      Just conta -> do
        newId <- runSQL $ insert conta
        json $ object ["result" .= String "success", "id" .= newId]
-- Fim endpoints conta

-- Endpoints operação financeira
  get "operacao-financeira" $ do
    operacoes <- runSQL $ selectList [] [Desc OperacaoFinanceiraId]
    json operacoes

  get ("operacao-financeira" <//> var) $ \operacaoId -> do
    maybeOperacao <- runSQL $ P.get operacaoId :: ApiAction (Maybe OperacaoFinanceira)
    case maybeOperacao of
      Nothing -> errorJson 404 "Could not find a OperacaoFinanceira entity with matching id."
      Just operacao -> json operacao

  post "operacao-financeira" $ do
    maybeOperacao <- jsonBody :: ApiAction (Maybe OperacaoFinanceira)
    case maybeOperacao of
      Nothing -> errorJson 500 "Failed to parse request body as OperacaoFinanceira"
      Just operacao -> do
        newId <- runSQL $ insert operacao
        json $ object ["result" .= String "success", "id" .= newId]

-- Fim endpoints operacao-financeira

-- Endpoint regras de negocio 
  get ("operacoes-por-conta-id" <//> var) $ \(contaId :: Int) -> do
    let contaIdKey = Just $ toSqlKey (fromIntegral contaId) :: Maybe (P.Key ContaCorrente)
    operacoes <- obterOperacoesConta contaIdKey
    json $ operacoes

  get ("saldo" <//> var) $ \(contaId :: Int) -> do
    let contaIdKey = Just $ toSqlKey (fromIntegral contaId) :: Maybe (P.Key ContaCorrente)
    operacoes <- obterOperacoesConta contaIdKey
    saldo <- calcularSaldo contaId operacoes
    json $ object ["result" .= String "success", "saldo" .= saldo]
  
  
  get ("conta-corrente-por-correntista" <//> var) $ \correntistaId -> do
    let correntistaIdKey = toSqlKey correntistaId :: P.Key Correntista
    contas <- runSQL $ selectList [ContaCorrenteCorrentistaId ==. correntistaIdKey] []
    json contas

  get "conta-corrente-por-num-conta" $ do
    maybeNumConta <- param "numConta"
    case maybeNumConta of
      Nothing -> json $ object [ "result" .= String "failure", "error" .= object ["code" .= String "500", "message" .= String "Número da conta é obrigatório"]]
      Just numConta -> do
        contas <- runSQL $ selectList [ContaCorrenteNumConta ==. numConta] []
        json contas