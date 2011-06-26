module Main (main) where

import System.Locale
import Data.Time


main = interact processStat


processStat :: String -> String
processStat = buildResult . parse . filter f . lines
    where f = (==) 'D' . head


parse :: [String] -> [(UTCTime, UTCTime)]
parse []     = []
parse (x:xs) = let both  = drop 1 $ words x
                   start = unwords $ fix $ take 5 both
                   end   = unwords $ fix $ drop 5 both
                   startTime = parseOne start
                   endTime   = parseOne end
                in (startTime, endTime) : parse xs
    where parseOne :: String -> UTCTime
          parseOne = readTime defaultTimeLocale "%d %m %Y %H %M"
          fix (d:m:y:h:i:[]) = (pad d)
                             : (pad $ show $ read m + 1)
                             : (show $ read y + 1900)
                             : (pad h)
                             : (pad i)
                             : []
          pad :: String -> String
          pad x = if length(x) == 1 then "0" ++ x else x


buildResult :: [(UTCTime, UTCTime)] -> String
buildResult = unlines . map f
    where f (s, e) = show s ++ " — " ++ show e
