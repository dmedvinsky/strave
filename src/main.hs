module Main (main) where

import Data.Time (readTime, UTCTime, NominalDiffTime, diffUTCTime)
import System.FilePath
import System.Environment (getProgName, getArgs, getEnv)
import System.IO
import System.Locale (defaultTimeLocale)


main :: IO ()
main = do
    args <- getArgs
    case args of
        []      -> doIt stdin
        "-h":_  -> usage
        "-d":_  -> defaultFilePath >>= doItFile
        file:_  -> doItFile file
  where
    doIt :: Handle -> IO ()
    doIt h = do
        input <- hGetContents h
        putStrLn $ processInput input
    doItFile :: FilePath -> IO ()
    doItFile path = withFile path ReadMode doIt
    usage :: IO ()
    usage = do
        name <- getProgName
        putStrLn $
            "Usage: " ++ name ++ " [-d|/path/to/historystats]\n\n\
                \    -h    Display this help and quit\n\
                \    -d    Default historystats ($HOME/.workrave/historystats) file is used\n\
                \    /path/to/historystats\n\
                \          Use path as historystats file\n\n\
                \    With no arguments stdin is read."


defaultFilePath :: IO FilePath
defaultFilePath = do
    home <- getEnv "HOME"
    return $ home </> ".workrave" </> "historystats"


processInput :: String -> String
processInput = buildResult . parse . filter f . lines
    where f = (==) 'D' . head


parse :: [String] -> [(UTCTime, UTCTime)]
parse []     = []
parse (x:xs) = let dates = drop 1 $ words x
                   start = parseUglyDate $ take 5 dates
                   end   = parseUglyDate $ drop 5 dates
                in (start, end) : parse xs

parseUglyDate :: [String] -> UTCTime
parseUglyDate = readTime defaultTimeLocale "%d %m %Y %H %M" . unwords . fix
    where fix (d:m:y:h:i:[]) = (pad d)
                             : (pad $ show $ read m + 1)
                             : (show $ read y + 1900)
                             : (pad h)
                             : (pad i)
                             : []

pad :: String -> String
pad x = if length(x) == 1 then '0' : x else x

minutes :: NominalDiffTime -> Int
minutes = truncate . (/ 60) . realToFrac

hours :: NominalDiffTime -> Int
hours = truncate . (/ 60) . fromIntegral . minutes

buildResult :: [(UTCTime, UTCTime)] -> String
buildResult = unlines . map f
    where f (s, e) = let date = head $ words $ show s
                         diff = e `diffUTCTime` s
                         hrs = hours diff
                         mins = (minutes diff) - hrs * 60
                      in date ++ " — " ++ pad (show hrs) ++ ":" ++ pad (show mins)
