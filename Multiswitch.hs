module Main where

import Prelude

import Data.List

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

import System.Random
import System.Environment


linkArray :: Integer -> String
linkArray n = foldr (\i rst ->
                       "      [\"h" ++ show i ++ "\", \"s1\"]"
                       ++ (if (i == n) then "" else  ",\n")
                       ++ rst
                    ) "" [1..n]

mkLinks :: Integer -> String
mkLinks n =
  "  \"links\": [\n" ++
     linkArray n ++
     "\n  ],\n"


hostArray :: Integer -> IO String
hostArray n =
  foldr (\i rst -> do
            h <- hostDescr i n
            r <- rst
            return (h ++ r)
        ) (return "") [1..n]
  where
    hostDescr :: Integer -> Integer -> IO String
    hostDescr i n = getNext i n >>= (\nxt -> return (
      "      \"h"++ show i ++ "\" : { \n        " ++
      "\"cmd\": \"ping -c 10 h" ++ nxt ++"\"\n      }" ++
      (if (i == n) then "" else ",\n")))

    getNext :: Integer -> Integer -> IO String
    getNext i n = (randomRIO (0,n) :: IO Integer) >>= return . show
     
      
  
mkHosts :: Integer -> IO String
mkHosts n = do
  hosts <- hostArray n
  return ("    \"hosts\": {\n" ++ hosts ++ "\n  },")

commands :: Integer -> String
commands n =
  foldr (\i rst ->
           let ip = "10.0." ++ show (i-1) ++ ".10" in
           let pt = show i in
           let macsfx = (\j -> if j <= 10 then "0" ++ show j else show j) in
           let fwdmac = "00:04:00:00:00:" ++ macsfx (i-1)in
           let sendmac = "00:aa:bb:00:00:" ++ macsfx (i-1) in
           "           \"table_add ipv4_lpm set_nhop " ++ ip ++ "/32 => " ++ ip ++ " " ++ pt ++ "\",\n" ++
           "           \"table_add forward set_dmac " ++ ip ++ " => " ++ fwdmac ++ "\",\n" ++
           "           \"table_add send_frame rewrite_mac " ++ pt ++ " => " ++ sendmac ++
           (if (i == n) then "\"\n" else "\",\n")
           ++ rst
           ) "" [1..n]


mkSwitches :: Integer -> String
mkSwitches n =
  "    \"switches\": {\n" ++
  "      \"s1\": {\n" ++
  "         \"commands\" : [\n" ++ commands n ++ "\n" ++
  "         ]\n" ++
  "      }\n" ++
  "    }\n"
  
mkAfter :: Integer -> String
mkAfter _ = ""

mkjson :: Integer -> IO String
mkjson n = do
  hosts <- mkHosts n
  return (
    "{ \"program\": \"monitor.p4, \n  \"language\":\"p4-16\"," ++
    "  \"targets\": {" ++
    "    \"multiswitch\": { \n" ++
    mkLinks n ++
    hosts ++
    mkSwitches n ++
    mkAfter n ++ 
    "\n}") --   \"parameters\": { \n"++
    -- "    \"port\": 8000, \n"++
    -- "    \"echo_msg\": \"foobar\"\n  }\n}")


mkMultiswitchConfig :: Integer -> IO ()
mkMultiswitchConfig n = mkjson n >>= putStrLn

main = do
  args <- getArgs
  mkMultiswitchConfig $ read $ head args
