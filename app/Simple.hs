{-# LANGUAGE OverloadedStrings #-}
module Main where

import KRPCHS (withRPCClient, withStreamClient, runRPCProg)

import KSP.Automation


main :: IO ()
main =
    withRPCClient "Low Kerbin Orbit" "127.0.0.1" "50000" $ \client ->
    withStreamClient client "127.0.0.1" "50001" $ \streamClient ->
        runRPCProg client (lowKerbinOrbit logStdOut streamClient)
