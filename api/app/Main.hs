{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson hiding (json)
import           Data.Text (Text, pack)
import           GHC.Generics

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)

instance ToJSON Person

instance FromJSON Person

data Correntista = Correntista
  { idCorrentista :: Text
  , nome :: Text
  , cpf :: Text
  , senha :: Text
  } deriving (Generic, Show)

instance ToJSON Correntista

instance FromJSON Correntista

data ContaCorrente = ContaCorrente
  { correntistaId :: Text
  , numConta :: Text
  , saldo :: Double
  } deriving (Generic, Show)

instance ToJSON ContaCorrente

instance FromJSON ContaCorrente

data OperacaoFinanceira = OperacaoFinanceira
  { contaCorrenteId :: Text
  , contaOrigemId :: Text
  , contaDestinoId :: Text
  , valor :: Double
  } deriving (Generic, Show)

instance ToJSON OperacaoFinanceira

instance FromJSON OperacaoFinanceira

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "people" $ do
    json [Person { name = "Fry", age = 25 }, Person { name = "Bender", age = 4 }]

  post "people" $ do
    thePerson <- jsonBody' :: ApiAction Person
    text $ "Parsed: " <> pack (show thePerson)

  get "correntistas" $ do
    json [ Correntista { idCorrentista = "1", nome = "Marli", cpf = "123456789", senha = "amarelo123" }
         , Correntista { idCorrentista = "2", nome = "Batista", cpf = "987654321", senha = "cantor457" }
         ]

  get "contas" $ do
    json [ ContaCorrente { correntistaId = "1", numConta = "1001", saldo = 1000.0 }
         , ContaCorrente { correntistaId = "2", numConta = "2001", saldo = 500.0 }
         ]

  post "operacoes" $ do
    op <- jsonBody' :: ApiAction OperacaoFinanceira
    text $ "Received operation: " <> pack (show op)
