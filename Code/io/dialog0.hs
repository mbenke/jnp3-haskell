main = do
  putStrLn "Hej, co powiesz?"
  input <- getLine
  putStrLn $ "Powiedziałeś: " ++ input
  putStrLn "Do widzenia"
