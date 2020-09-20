{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Text.Printf
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified 
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Builder (Builder, fromText, singleton)
import Data.Text.Lazy.Builder as TB
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.IO qualified
import Data.Text.Lazy.Encoding qualified
import Data.Void
import System.Environment
import System.IO (IOMode (WriteMode), withFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as M
import Control.Applicative (liftA2)
import Data.Foldable qualified
import Control.Exception

data Pair a = Pair a a deriving (Functor, Show)

type Parser = Parsec Void Text

subtitlesParser :: Parser ([Subtitle (Pair Moment)])
subtitlesParser = do
  r <-
    some
      ( do
          index <- L.decimal <* hspace <* eol
          interval <- intervalParser
          lines <- some lineParser
          some blankLine
          pure $ Subtitle index lines interval
      )
  eof
  pure r
  where
    blankLine = do
      hspace
      eol
    lineParser = do
      hspace
      line <- takeWhile1P Nothing (\c -> c /= '\n' && c /= '\r')
      eol
      return line
    intervalParser :: Parser (Pair Moment)
    intervalParser = do
      start <- momentParser
      string "-->"
      hspace
      end <- momentParser
      hspace
      eol
      pure $ Pair start end
    momentParser :: Parser Moment
    momentParser = do
      hours' <- L.decimal
      char ':'
      minutes' <- L.decimal
      char ':'
      seconds' <- L.decimal
      char ','
      millis' <- L.decimal
      hspace
      pure $ Moment hours' minutes' seconds' millis'

data Moment = Moment
  { hours :: Int,
    minutes :: Int,
    seconds :: Int,
    millis :: Int
  }
  deriving (Show)

newtype Millis = Millis Int deriving (Show)

toMillis :: Moment -> Millis
toMillis (Moment {hours,minutes,seconds,millis}) = Millis $
    millis +
    seconds * 1e3 +
    minutes * 6e1 * 1e3 +
    hours * 6e1 * 6e1 * 1e3

toMoment :: Millis -> Moment
toMoment (Millis m) = 
    let (hours,minutes') = m `quotRem` (6e1 * 6e1 * 1e3)
        (minutes,seconds') = minutes' `quotRem` (6e1 * 1e3)
        (seconds,millis) = seconds' `quotRem` 1e3
     in Moment {hours,minutes,seconds,millis}
    

data Subtitle a = Subtitle
  { subtitleId :: Int,
    subtitleLines :: [Text],
    subtitleInterval :: a
  }
  deriving (Functor,Show)

subtitleMap :: Foldable f  => f (Subtitle a) -> IntMap (Subtitle a)
subtitleMap c = M.fromList $ liftA2 (,) subtitleId id <$> Data.Foldable.toList c  

renderSubtitle ::
  Subtitle (Pair Moment) ->
  Builder
renderSubtitle (Subtitle {subtitleId, subtitleLines, subtitleInterval = Pair beginning end}) =
  decimal subtitleId
    <> newline
    <> renderMoment beginning
    <> fromText " --> "
    <> renderMoment end
    <> newline
    <> foldMap (\l -> fromText l <> newline) subtitleLines
    <> newline
  where
    newline = TB.singleton '\n'

renderManySubtitles :: [Subtitle (Pair Moment)] -> Builder
renderManySubtitles = foldMap renderSubtitle

renderMoment :: Moment -> Builder
renderMoment (Moment {hours, minutes, seconds, millis}) =
  fromString (printf "%02d" hours)
    <> singleton ':'
    <> fromString (printf "%02d" minutes)
    <> singleton ':'
    <> fromString (printf "%02d" seconds)
    <> singleton ','
    <> fromString (printf "%03d" millis)

main :: IO ()
main = do
  [source, rule, dest] <- getArgs
  sourceText <- do
    bytes <- B.readFile source
    pure $ decodeUtf8 bytes
  let parseResult = runParser subtitlesParser source sourceText
  case parseResult of
    Left err -> print err
    Right subtitles -> do
      ruleText <- do
        bytes <- B.readFile rule
        pure $ decodeUtf8 bytes
      let ruleResult = runParser subtitlesParser rule ruleText
      case ruleResult of
        Left err -> print err
        Right (rsub1 : rsub2 : _) -> do
          let adjusted = adjust (fmap (fmap (fmap toMillis)) subtitles) (fmap (fmap toMillis) rsub1) (fmap (fmap toMillis) rsub2)
              renderedResult = renderManySubtitles $ fmap (fmap (fmap toMoment)) adjusted
          -- Data.Text.Lazy.IO.putStr (toLazyText renderedResult)
          Data.ByteString.Lazy.writeFile dest (Data.Text.Lazy.Encoding.encodeUtf8 (toLazyText renderedResult))

adjust :: [Subtitle (Pair Millis)] -> Subtitle (Pair Millis) -> Subtitle (Pair Millis) -> [Subtitle (Pair Millis)]
adjust subs rule1@(Subtitle {subtitleInterval = Pair (Millis ruleStart) _}) 
            rule2@(Subtitle {subtitleInterval = Pair (Millis ruleEnd) _})  = 
    let subsById = subtitleMap subs 
        Just (Subtitle {subtitleInterval = Pair (Millis origStart) _}) = M.lookup (subtitleId rule1) subsById
        Just (Subtitle {subtitleInterval = Pair (Millis origEnd) _}) = M.lookup (subtitleId rule2) subsById
        origDx = origEnd - origStart
        ruleDx = ruleEnd - ruleStart
        displacement = ruleStart - origStart
        increment :: Rational = toRational ruleDx / toRational origDx
        adjustOne one@(Subtitle {subtitleInterval = Pair (Millis oneStart) (Millis oneEnd)}) =
            let width = oneEnd - oneStart
                remap :: Rational = toRational (oneStart - origStart) * increment + toRational origStart + toRational displacement
             in one { subtitleInterval = Pair (Millis $ round remap) (Millis $ round remap + width) }
     in adjustOne <$> subs

