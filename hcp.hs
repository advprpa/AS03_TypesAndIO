import System.Environment

main :: IO ()
main = do 
    [source, target] <- getArgs -- Without error handling
    content <- readFile source
    writeFile target content
