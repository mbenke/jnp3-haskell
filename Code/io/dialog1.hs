import System.IO

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    hFlush stdout
    getLine

main = do
  input <- promptLine "Prompt>"
  putStrLn $ "Powiedziałeś: " ++ input
