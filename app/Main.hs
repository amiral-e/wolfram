--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Main
--

module Main where

import Lib ( checkArgs, checkFlags, defaultConf, wolfram )
import System.Environment ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    checkArgs args
    wolfram (checkFlags args (Just defaultConf))
