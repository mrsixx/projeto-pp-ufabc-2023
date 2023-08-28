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

module Lib
    ( calcularSaldo ) where
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
import           Models
import Control.Monad.IO.Class (liftIO)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type ApiAction a = SpockAction SqlBackend () () a

getValor = operacaoFinanceiraValor . entityVal
getOrigemId = operacaoFinanceiraContaOrigemId . entityVal

-- baseado em quem está consultando o saldo, determina se o valor de uma operação é entrada ou saída
getValorOperacao :: Int -> (Entity OperacaoFinanceira -> Double)
getValorOperacao contaId = \operacao -> do
    let maybeOrigemId = getOrigemId operacao
    let contaId' = toSqlKey (fromIntegral contaId)
    case maybeOrigemId of
        Just origemId | contaId' == origemId -> -1 * (getValor operacao) -- Dinheiro saindo
        _ -> 1 * (getValor operacao)                                    -- Dinheiro entrando

-- Calcula o saldo de um correntista baseado nas operações financeiras
calcularSaldo :: Int -> [Entity OperacaoFinanceira] -> ApiAction Double
calcularSaldo id operacoes = do
    let valores = map (getValorOperacao id) operacoes
    return (foldr (+) 0 valores)