--
-- EPITECH PROJECT, 2022
-- Untitled (Workspace)
-- File description:
-- Lib
--

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib ( checkArgs, checkFlags, defaultConf, wolfram ) where

import System.Environment ( getArgs )
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Data.Maybe ( isJust )
import Text.Read ( readMaybe )

data Conf = Conf {
    rule :: Int,
    start :: Int,
    count :: Int,
    window :: Int,
    move :: Int,
    offset :: Int,
    space :: String
}

defaultConf :: Conf
defaultConf = Conf {rule = 0, start = 0, count = -1,
    window = 80, move = 0, offset = 0, space = ""}

isFlag :: String -> Bool
isFlag x = x `elem` ["--rule", "--start", "--lines", "--window", "--move"]

displayError :: String -> IO ()
displayError error = putStrLn error >>
    exitWith (ExitFailure 84)

rule30 :: String -> String -> String
rule30 ('*':'*':'*':x) y = rule30 ("**" ++ x) (y ++ " ")
rule30 ('*':'*':' ':x) y = rule30 ("* " ++ x) (y ++ " ")
rule30 ('*':' ':'*':x) y = rule30 (" *" ++ x) (y ++ " ")
rule30 ('*':' ':' ':x) y = rule30 ("  " ++ x) (y ++ "*")
rule30 (' ':'*':'*':x) y = rule30 ("**" ++ x) (y ++ "*")
rule30 (' ':'*':' ':x) y = rule30 ("* " ++ x) (y ++ "*")
rule30 (' ':' ':'*':x) y = rule30 (" *" ++ x) (y ++ "*")
rule30 (' ':' ':' ':x) y = rule30 ("  " ++ x) (y ++ " ")
rule30 _ x = x ++ "  "

rule90 :: String -> String -> String
rule90 ('*':'*':'*':x) y = rule90 ("**" ++ x) (y ++ " ")
rule90 ('*':'*':' ':x) y = rule90 ("* " ++ x) (y ++ "*")
rule90 ('*':' ':'*':x) y = rule90 (" *" ++ x) (y ++ " ")
rule90 ('*':' ':' ':x) y = rule90 ("  " ++ x) (y ++ "*")
rule90 (' ':'*':'*':x) y = rule90 ("**" ++ x) (y ++ "*")
rule90 (' ':'*':' ':x) y = rule90 ("* " ++ x) (y ++ " ")
rule90 (' ':' ':'*':x) y = rule90 (" *" ++ x) (y ++ "*")
rule90 (' ':' ':' ':x) y = rule90 ("  " ++ x) (y ++ " ")
rule90 _ x = x ++ "  "

rule110 :: String -> String -> String
rule110 ('*':'*':'*':x) y = rule110 ("**" ++ x) (y ++ " ")
rule110 ('*':'*':' ':x) y = rule110 ("* " ++ x) (y ++ "*")
rule110 ('*':' ':'*':x) y = rule110 (" *" ++ x) (y ++ "*")
rule110 ('*':' ':' ':x) y = rule110 ("  " ++ x) (y ++ " ")
rule110 (' ':'*':'*':x) y = rule110 ("**" ++ x) (y ++ "*")
rule110 (' ':'*':' ':x) y = rule110 ("* " ++ x) (y ++ "*")
rule110 (' ':' ':'*':x) y = rule110 (" *" ++ x) (y ++ "*")
rule110 (' ':' ':' ':x) y = rule110 ("  " ++ x) (y ++ " ")
rule110 _ x = x ++ "  "

callRule :: Int -> String -> String
callRule 30 x = rule30 x "  "
callRule 90 x = rule90 x "  "
callRule 110 x = rule110 x "  "

genSpace :: Int -> String
genSpace x
    | x <= 0 = []
    | otherwise = replicate x ' '

setConf :: Conf -> Conf
setConf cfg = cfg {offset = window cfg `div` 2 - 2,
    space = genSpace (window cfg `div` 2 - 2)}

modConf :: Conf -> Conf
modConf cfg
    | null (space cfg) = cfg {offset = offset cfg - 1, count = count cfg - 1}
    | otherwise = cfg {offset = offset cfg - 1, count = count cfg - 1,
        space = tail (space cfg)}

skipConf :: Conf -> Conf
skipConf cfg
    | null (space cfg) = cfg {start = start cfg - 1}
    | otherwise = cfg {start = start cfg - 1, space = tail (space cfg)}

frontSpace :: String -> Int -> String
frontSpace x 0 = x
frontSpace x y = frontSpace (" " ++ x) (y - 1)

rmBack :: String -> Int -> String
rmBack x 0 = x
rmBack x y = rmBack (tail x) (y - 1)

rmFront :: String -> Int -> String
rmFront x 0 = x
rmFront x y = rmFront (init x) (y - 1)

moveLine :: Conf -> String -> String
moveLine cfg line
    | move cfg > 0 = frontSpace line (move cfg - 1)
    | move cfg < 0 = rmBack line (-(move cfg + 1))
    | otherwise = line

resizeLine :: Int -> Int -> String -> String
resizeLine len window line = rmBack (rmFront line
        ((len - window) `div` 2 + 1)) ((len - window) `div` 2)

printLine :: Conf -> String -> IO ()
printLine cfg line
    | null (space cfg) = putStrLn (moveLine cfg
        (resizeLine (length line) (window cfg) line))
    | otherwise = putStrLn (moveLine cfg (space cfg ++ line ++
        tail (space cfg)))

genLine :: Conf -> String -> IO ()
genLine cfg line
    | count cfg == 0 = return ()
    | otherwise = printLine cfg line >>
        genLine (modConf cfg) (callRule (rule cfg) line)

skipLine :: Conf -> String -> (Conf, String)
skipLine cfg line
    | start cfg == 0 = (cfg, line)
    | otherwise = skipLine (skipConf cfg) (callRule (rule cfg) line)

wolfram :: Maybe Conf -> IO ()
wolfram Nothing = displayError "program couldn't parse arguments"
wolfram (Just cfg) =
    let (conf, line) = skipLine (setConf cfg) "  *  "
    in genLine conf line

isPos :: Maybe Int -> Maybe Int
isPos Nothing = Nothing
isPos (Just x)
    | x >= 0 = Just x
    | otherwise = Nothing

parseConf :: Int -> Maybe Int -> Conf -> Maybe Conf
parseConf _ Nothing _ = Nothing
parseConf 0 (Just val) cfg
    | val `notElem` [30, 90, 110] = Nothing
    | otherwise = Just cfg {rule = val}
parseConf 1 (Just val) cfg = Just cfg {start = val}
parseConf 2 (Just val) cfg = Just cfg {count = val}
parseConf 3 (Just val) cfg = Just cfg {window = val}
parseConf 4 (Just val) cfg = Just cfg {move = val}

maybeConf :: Int -> String -> Conf -> Maybe Conf
maybeConf 0 y cfg = parseConf 0 (readMaybe y::Maybe Int) cfg
maybeConf 1 y cfg = parseConf 1 (isPos (readMaybe y::Maybe Int)) cfg
maybeConf 2 y cfg = parseConf 2 (isPos (readMaybe y::Maybe Int)) cfg
maybeConf 3 y cfg = parseConf 3 (isPos (readMaybe y::Maybe Int)) cfg
maybeConf 4 y cfg = parseConf 4 (readMaybe y::Maybe Int) cfg

checkFlags :: [String] -> Maybe Conf -> Maybe Conf
checkFlags [] cfg = cfg
checkFlags _ Nothing = Nothing
checkFlags ("--rule":xs:y) (Just cfg) = checkFlags y (maybeConf 0 xs cfg)
checkFlags ("--start":xs:y) (Just cfg) = checkFlags y (maybeConf 1 xs cfg)
checkFlags ("--lines":xs:y) (Just cfg) = checkFlags y (maybeConf 2 xs cfg)
checkFlags ("--window":xs:y) (Just cfg) = checkFlags y (maybeConf 3 xs cfg)
checkFlags ("--move":xs:y) (Just cfg) = checkFlags y (maybeConf 4 xs cfg)
checkFlags (_:xs:y) (Just cfg) = Nothing

checkArgs :: [String] -> IO ()
checkArgs [] = displayError "program must take at least 2 arguments"
checkArgs [_] = displayError "program must take at least 2 arguments"
checkArgs args
    | odd (length args) = displayError "number of arguments must be even"
    | "--rule" `notElem` args = displayError "missing --rule setting"
    | otherwise  = return ()
