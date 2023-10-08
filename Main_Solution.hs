module Main where

import System.Environment
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.Encoding as E

-- cabal repl
-- :main Arg1
-- :set args Arg1
-- cabal run webclient Arg1
main :: IO ()
main = do
    args <- getArgs
    case interpretArgs args of
        Help -> putStrLn helpText
        LoadWeather city -> do
            result <- queryService (buildURL city)
            L8.putStrLn result


queryService :: String -> IO L8.ByteString
queryService url = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    pure (responseBody response)
    -- pure $ T.unpack (E.decodeUtf8 (responseBody response))

buildURL :: String -> String
buildURL location = "https://wttr.in/~" ++ location ++ "?format=3"

data Action = Help | LoadWeather String

helpText :: String
helpText = concat [
     "wttr queries the current weather for a given city name.\n",
     "Example usage:\n> wttr Zurich"
  ]

interpretArgs :: [String] -> Action
interpretArgs []         = Help
interpretArgs ["--help"] = Help
interpretArgs ["-h"]     = Help
interpretArgs [city]     = LoadWeather city
interpretArgs _          = Help
