module Main where

import Prelude

import Data.List
import Data.Bits

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

import System.Random
import System.Environment
import System.Random.Shuffle

p4appCommands :: Integer -> Integer -> IO String
p4appCommands n numHH = do
  commslst <- shuffleM $ map (\ i ->  hostDescr i n numHH ) [1..n]
  let commsstr = (foldr (\i rst -> i++rst) "" commslst) ++ (suffix n numHH)
  return commsstr
  where
    hostDescr :: Integer -> Integer -> Integer -> String
    hostDescr i n numHH =
      let nxt = (i `mod` numHH) + 1 in
      "sudo p4app exec m " ++ "h"++ show i ++
      " ping -c 10 "++ getIP nxt ++" & \n"

p4appCommRandom :: Integer -> Integer -> IO String
p4appCommRandom n numHH = 
  foldr (\i rst -> do
         h <- hostDescr i n numHH
         r <- rst
         return (h ++ r)) (return $ suffix n numHH) [1..n]
  where
    hostDescr :: Integer -> Integer -> Integer -> IO String
    hostDescr i n numHH = do
      nxt <-  randomRIO(1,numHH)
      return ("sudo p4app exec m " ++ "h"++ show i ++ " ping -c 10 "++ getIP nxt ++" & \n")

suffix :: Integer -> Integer -> String
suffix n numHH =
  "sleep 1s\n" ++
  "p4app exec m s1 \"echo register_read hashedKey | simple_switch_CLI\" >> " ++
  "./tests/h"++show n
  ++ "hh" ++ show numHH ++ "arr30.dmp"
    
  


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


p4appJSON :: Integer -> String
p4appJSON n =
  "{ \"program\": \"monitor.p4\",\n" ++
  "  \"language\": \"p4-16\",\n" ++
  "  \"targets\": {\n" ++
  "    \"mininet\" : {\n" ++
  "       \"num-hosts\": " ++ show n ++ ",\n" ++
  "       \"switch-config\": \"s1.config\"\n" ++
  "     }\n"++
  "   }\n "++
  "}"


getIPS :: [Integer] -> [String]
getIPS ips =
  map (\ip ->
          intercalate "."
          $ foldr (\sft rst ->
                      show (shiftR (shiftL (255::Integer) sft .&. ip) sft) :
                      rst) [] [24,16,8,0]
      ) ips 

testHHS :: [Integer] -> Int -> Int
testHHS ips numHH =
  let groundTruth = map (\i -> "10.0." ++ show i ++ ".10") [0..numHH-1] in
  numHH - (length ( groundTruth \\ getIPS ips))

main = do
  args <- getArgs
  let count = (read $ head args) :: Integer
  let numHH = (read $ head $ tail args) :: Integer
  p4app <- p4appCommands count numHH --p4appCommRandom count numHH 
  putStrLn "P4APP SCRIPT"
  writeFile "./tests/runtest" $ p4app
  putStrLn "CLI SCRIPT" 
  writeFile "./monitor.p4app/s1.config" (switchCommands count)
  writeFile "./monitor.p4app/p4app.json" (p4appJSON count)
  putStrLn "Run these commands: "
  putStrLn "p4app exec m s1 simple_switch_CLI "
  putStrLn "register_read packetCounts"
  putStrLn "register_read hashedKeys"
  putStrLn "register_read validBit"
