{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Warp.Packet ( ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Data.Binary
import Control.Lens hiding (noneOf)

import Warp.Routing.Route
import Warp.Routing.Graph
import Warp.Types
import ADN.Core
import ADN.Debug
import Text.Read (readMaybe)
import Data.Tree
import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator


{-
syntax: <PATH> <ROUTE> <PAYLOAD>
example: the following command builds a packet with
            PATH= [3,5,9]
            ROUTE=         2     1
                          /\     /\
                         4  6   3  4
            PAYLOAD = hello world!
                   
       > [3 5 9]  [(2 (4 6)) (1 (3 4))] "hello world!"
-}

instance DebugPacket WarpPkt where 
    dbgRead str = case parse parsePacket "readPacket" str of
                    Left err -> Nothing
                    Right pkt -> pure pkt
    dbgShow p = show p ++ "\tPAYLOAD: " ++  dbgShow (_payload p)

instance DebugPacket WarpContents where
    dbgRead str = case parse parseWarpContent "readWarpContent" str of
                        Left err -> Nothing
                        Right pkt -> pure pkt 
    dbgShow (WarpSearch searchpacket) = show searchpacket
    dbgShow (WarpPayload pl) = dbgShow pl

instance Show WarpPkt where
    show (WarpPkt p d pl) = "SOURCE: " ++ show p ++ "\tDEST: " ++ dest 
        where dest = case d of Left _ -> "broadcast" 
                               Right route -> show route



instance DebugPacket WarpMsg where 
    dbgRead s | null s = Nothing
                | otherwise = let ws = words s in do
        p <- dbgRead . unwords $ tail ws
        i <- readMaybe (head ws) :: Maybe Int
        return $ WarpMsg (WID i) p
    dbgShow p = show p ++ "\tPAYLOAD: " ++ dbgShow (_wmPayload p)

instance Show WarpMsg where show (WarpMsg d p) = "SOURCE: " ++ show d
    


parseInt :: Parser Int
parseInt = read <$> many1 digit

parseWID :: Parser WID
parseWID = WID . read <$> many1 digit
parseTree :: Parser (Tree WID)
parseTree = parseLeaf <|> parseForest
    where parseLeaf = Node <$> parseWID <*> pure []
          parseForest = between (spaces >> char '(' >> spaces) (spaces >> char ')' >> spaces) $ do 
                                                           uid <- parseWID 
                                                           spaces
                                                           forest <- parseTree `sepBy` spaces
                                                           pure $ Node uid forest

parseRoute :: Parser (Either TTL [Route])
parseRoute = 
                (Left . TTL <$> parseInt) -- broadcast (TTL)
                <|> (Right . fmap R <$> between (char '[' >> spaces) (spaces >> char ']') (parseTree `sepBy` spaces)) -- [Trees]
parsePath = between (char '[' >> spaces) (spaces >> char ']') $ parseWID `sepBy` spaces

parsePayload :: Parser String
parsePayload = between (char '"' >> spaces) (spaces >> char '"') $ many1 (noneOf $ "\"")

parseSearchPkt :: Parser SearchPkt
parseSearchPkt = parseAsk <|> parseTell
    where parseAsk = do string "Ask" 
                        spaces
                        res <- parseInt
                        pure $ Ask (Res $ RID res)
          parseTell = do string "Tell"
                         spaces
                         rid <- parseInt
                         pure $ Tell $ Just $ RID rid
parseWarpContent = (WarpSearch <$> parseSearchPkt) <|> (WarpPayload . Payload . BL.toStrict . encode  <$> parsePayload)

parsePacket = do
    path <- parsePath
    spaces
    route <- parseRoute
    spaces
    pl <- parseWarpContent
--    pure $ WarpPkt path route $ WarpPayload (Payload. BL.toStrict $ encode pl)
    pure $ WarpPkt path route pl



