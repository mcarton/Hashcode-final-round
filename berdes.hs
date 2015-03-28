import System.Environment
import Control.Monad

main :: IO ()
main = do
  (fileName:_) <- getArgs
  (infos:datas) <- liftM (lines) $ readFile fileName
  let [] = map (read :: String -> Int) $ words infos
  return ()
