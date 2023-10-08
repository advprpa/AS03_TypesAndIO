import System.Environment
import Data.ByteString.Lazy as BL

main :: IO ()
main = do 
    [source, target] <- getArgs
    content <- BL.readFile source
    BL.writeFile target content
