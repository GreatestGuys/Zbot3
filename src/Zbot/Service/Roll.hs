{-# LANGUAGE OverloadedStrings #-}
module Zbot.Service.Roll (
    roll
,   exprParser
,   Expr(..)
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Message
import Zbot.Extras.Command
import Zbot.Extras.UnitService

import Control.Applicative ((<|>))
import Control.Monad (liftM2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)

import Data.Char
import System.Random

import qualified Data.Attoparsec.Expr as E
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T


-- | A service that will roll a dice when a user messages "!roll".
roll :: (MonadIO m, Bot m) => Service m ()
roll = unitService "Zbot.Service.Roll" (onCommand "!roll" handleCommand)

handleCommand :: (MonadIO m, Bot m)
              => MessageContext m -> T.Text -> MonadService () m ()
handleCommand ctx arg = lift $ rollDice (reply ctx) arg

rollDice :: (MonadIO m, Bot m) => (T.Text -> m ()) -> T.Text -> m ()
rollDice reply arg | all isDigit (T.unpack arg)
                   = doRight $ Die 1 (read $ T.unpack arg :: Int)
                   | otherwise
                   = either (const $ return ()) doRight parse
    where parse = Atto.parseOnly exprParser arg
          doRight r = do
            result <- eval r
            reply (T.pack $ show result)

-- | Parsing
data Expr = Const Int
          | Die Int Int
          | Expr :+: Expr
          | Expr :-: Expr
          | Negate Expr
          deriving (Eq, Show)

table = [ [prefix "-" Negate]
        , [binary "+" (:+:) E.AssocLeft, binary "-" (:-:) E.AssocLeft]
        ]
    where
        prefix name f = E.Prefix (do{ Atto.string name; return f })
        binary name f = E.Infix (do{ Atto.string name; return f })

constantParser :: Atto.Parser Expr
constantParser = do
  i <- Atto.decimal
  return $ Const i

dieParser :: Atto.Parser Expr
dieParser = do
  n <- Atto.decimal
  Atto.char 'd'
  s <- Atto.decimal
  return $ Die n s

exprParser :: Atto.Parser Expr
exprParser = E.buildExpressionParser table atomParser
    where
        atomParser =   Atto.skipSpace *> ((Atto.skipSpace *> dieParser <* Atto.skipSpace)
                   <|> (Atto.skipSpace *> constantParser <* Atto.skipSpace)) <* Atto.skipSpace

eval :: (MonadIO m) => Expr -> m Int
eval (Const i) = return i
eval (Die n s) = do
          gen <- liftIO newStdGen
          let rolls = randomRs (1, s) gen
          return $ sum $ take n rolls
eval (e1 :+: e2) = liftM2 (+) (eval e1) (eval e2)
eval (e1 :-: e2) = liftM2 (-) (eval e1) (eval e2)
eval (Negate e)  = negate <$> eval e
