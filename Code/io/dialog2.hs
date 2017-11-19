import System.IO

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine

main = do
  hSetBuffering stdout NoBuffering
  input <- promptLine "Prompt> "
  putStrLn $ "Powiedziałeś: " ++ input
