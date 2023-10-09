module Main where

import System.Environment
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.Encoding as E


data Action = Help | LoadWeather String

newtype URL = URL String


interpretArgs :: [String] -> Action
interpretArgs []         = Help
interpretArgs ["--help"] = Help
interpretArgs ["-h"]     = Help
interpretArgs [city]     = LoadWeather city
interpretArgs _          = Help


dispatch :: Action -> IO ()
dispatch Help = showHelpAction
dispatch (LoadWeather city) = loadWeatherAction (buildURL city)


helpText :: String
helpText = concat [
     "wttr queries the current weather for a given city name.\n",
     "Example usage:\n> wttr Zurich"
  ]


showHelpAction :: IO ()
showHelpAction = putStrLn helpText


buildURL :: String -> URL
buildURL location = URL $ "https://wttr.in/~" ++ location ++ "?format=3"


loadWeatherAction :: URL -> IO ()
loadWeatherAction url = do
    result <- queryService url
    L8.putStrLn result


queryService :: URL -> IO L8.ByteString
queryService (URL url) = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    pure (responseBody response)
    -- pure $ T.unpack (E.decodeUtf8 (responseBody response))


-- cabal repl
-- :main Arg1
-- :set args Arg1
-- cabal run webclient Arg1
main :: IO ()
main = do
    args <- getArgs
    dispatch $ interpretArgs args
