import System.IO
main = do
   hSetBuffering stdin NoBuffering
   hSetBuffering stdout NoBuffering
   input <- getContents
   go input where 
     go [] = putStr "\nEOF\n"
     go ('q':_) = putStr "\nQUIT\n"
     go (c:cs) = putChar c >> go cs
