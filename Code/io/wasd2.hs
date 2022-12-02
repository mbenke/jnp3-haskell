import System.IO

doesQuit :: Char -> Bool
doesQuit 'q' = True
doesQuit _ = False

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    hFlush stdout
    getLine

putPrompt :: String -> IO ()
putPrompt prompt = do
    putStr prompt
    hFlush stdout

clearScreen :: IO ()
clearScreen =   putStr "\ESCc"

main = mainLoop
mainLoop :: IO()
mainLoop = do
  clearScreen
  hSetBuffering stdin NoBuffering
  allInput <- getContents
  next allInput
  where
    next s = putPrompt "> " >> go s 
    go :: String -> IO ()  
    go [] = return ()
    go (c:cs)  = do
      if doesQuit c
        then putStrLn " Sayonara." >> return ()
        else do
        processInput c >> next cs

processInput :: Char -> IO ()
processInput c = clearScreen >> putStrLn (' ':go c) where
  go 'w' = "Up"
  go 'a' = "Left"
  go 's' = "Down"
  go 'd' = "Right"
  go _ = "Huh?"

