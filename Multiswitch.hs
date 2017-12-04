module Main where

import Prelude

import Data.List

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

import System.Random
import System.Environment

p4appCommands :: Integer -> IO String
p4appCommands n =
  foldr (\i rst -> do
            h <- hostDescr i n
            r <- rst
            return (h ++ r)
        ) (return "") [1..n]
  where
    hostDescr :: Integer -> Integer -> IO String
    hostDescr i n = getNext i (2 * n `div` 3) >>= (\nxt -> return (
      "sudo p4app exec m " ++ "h"++ show i ++ " ping -c 10 "++ getIP nxt ++" & \n"))
    getNext :: Integer -> Integer -> IO Integer
    getNext i n = (randomRIO (1,n) :: IO Integer) >>= return


switchCommands :: Integer -> String
switchCommands n =
  "table_set_default send_frame _drop\n" ++
  "table_set_default forward _drop\n" ++
  "table_set_default ipv4_lpm _drop\n" ++
  foldr (\i rst ->
           let pt = show i in
           let macsfx = (\j -> if j <= 10 then "0" ++ show j else show j) in
           let fwdmac = "00:04:00:00:00:" ++ macsfx (i-1)in
           let sendmac = "00:aa:bb:00:00:" ++ macsfx (i-1) in
           "table_add ipv4_lpm set_nhop " ++ getIP i ++ "/32 => " ++ getIP i ++ " " ++ pt ++ "\n" ++
           "table_add forward set_dmac " ++ getIP i ++ " => " ++ fwdmac ++ "\n" ++
           "table_add send_frame rewrite_mac " ++ pt ++ " => " ++ sendmac ++ "\n"
           ++ rst
           ) "" [1..n]

getIP :: Integer -> String
getIP i = "10.0." ++ show (i-1) ++ ".10"

main = do
  args <- getArgs
  let count = (read $ head args) :: Integer
  p4cmds <- p4appCommands count
  putStrLn "P4APP SCRIPT"
  writeFile "./tests/runtest"  p4cmds
  putStrLn "CLI SCRIPT" 
  writeFile "./monitor.p4app/s1.config" (switchCommands count)
  putStrLn "Run these commands: "
  putStrLn "p4app exec m s1 simple_switch_CLI "
  putStrLn "register_read packetCounts"
  putStrLn "register_read hashedKeys"
  putStrLn "register_read validBit"
