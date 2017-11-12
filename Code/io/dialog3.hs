doesQuit :: String -> Bool
doesQuit "q" = True
doesQuit "quit" = True
doesQuit _ = False

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine

main = mainLoop
mainLoop :: IO()
mainLoop = do
  input <- promptLine "> "
  if doesQuit input
     then return ()
     else processInput input >> mainLoop

processInput :: String -> IO ()
processInput input = 
  putStrLn $ "Powiedziałeś: " ++ input

