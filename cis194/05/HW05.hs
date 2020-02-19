{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import System.FilePath.Posix
import Data.Bits (xor)
import Data.Maybe
import Control.Arrow ((&&&))

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret f g = do
  orig <- BS.readFile f
  modif <- BS.readFile g
  return $ BS.pack $ filter (/= 0) $ BS.zipWith xor orig modif

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key f = do
  enc <- BS.readFile $ f <.> ".enc"
  BS.writeFile f $ BS.pack $ BS.zipWith xor enc (BS.cycle key)


-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile f = do
  content <- BS.readFile f
  return $ decode content 

-- Exercise 4 -----------------------------------------

getBadTransactions  :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
getBadTransactions _ Nothing = Nothing
getBadTransactions Nothing _ = Nothing
getBadTransactions (Just v) (Just t) = Just $ filter (\x -> tid x `elem` v) t 

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims transaction = do
  v <- parseFile victims :: IO (Maybe [TId])
  t <- parseFile transaction :: IO (Maybe [Transaction])
  return $ getBadTransactions v t

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow t = Map.fromListWith (+) $ getFrom t ++ getTo t
  where
    getFrom = map (from &&& (\x -> -amount x))
    getTo = map (to &&& amount)


-- Exercise 6 -----------------------------------------
compAmount x y = snd x `compare` snd y

type PayMap = Map String Integer

getCriminal :: PayMap -> String
getCriminal m = fst $ head $ filter (== maxVal) sorted
  where 
    sorted = sortBy compAmount $ Map.toList m
    maxVal = last sorted

-- Exercise 7 -----------------------------------------

splitByAmount cmp cmp2 m = sortBy cmp $ Map.toList $ Map.filter cmp2 m

update' p money = Map.adjust (flip (-) money) p

type Due = (String, Integer)

revertFlow :: [Due] -> [Due] -> [TId] -> PayMap -> ([Transaction], PayMap)
revertFlow _ [] _ m  = ([], m)
revertFlow [] _ _ m  = ([], m)
revertFlow (owned:ps) (owner:qs) (t:ts) m  = ( tr : tr', m'')
  where
    money = min (snd owned) (-snd owner)
    m' = update' (fst owned) money . update' (fst owner) (-money) $ m
    (tr', m'') = revertFlow ps qs ts m'
    tr = Transaction { from = fst owned
                      , to = fst owner 
                      , amount = money
                      , tid = t
                      }

undoTs :: PayMap -> [TId] -> [Transaction]
undoTs m t 
  | null owneds || null owners = []
  | otherwise = tr ++ undoTs (Map.filter (/= 0) m') t''
  where
    owneds = splitByAmount (flip compAmount) (> 0) m
    owners = splitByAmount compAmount (< 0) m
    n = length owneds
    (t', t'') = splitAt n t
    (tr, m') = revertFlow owneds owners t' m

getMap = do
  trans <- parseFile "transactions.json" :: IO (Maybe [Transaction])
  return $ getFlow $ fromJust trans

test = do
  m <- getMap
  let m2 = undoTs m (repeat "")
  return m2

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON f content = do
  BS.writeFile f (encode content)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn $ "Criminal is=" ++ crim

