import System.IO

doesQuit :: Char -> Bool
doesQuit 'q' = True
doesQuit _ = False

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine

main = mainLoop
mainLoop :: IO()
mainLoop = do
  hSetBuffering stdin NoBuffering
  input <- getChar
  if doesQuit input
     then putStrLn " Sayonara." >> return ()
     else processInput input >> mainLoop

processInput :: Char -> IO ()
processInput c = putStrLn (' ':go c) where
  go 'w' = "Up"
  go 'a' = "Left"
  go 's' = "Down"
  go 'd' = "Right"
  go _ = "Huh?"

