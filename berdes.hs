import System.Environment
import Control.Monad

main :: IO ()
main = do
  (fileName:_) <- getArgs
  (sizes:nbrs:depart:datas) <- liftM (lines) $ readFile fileName
  let [nbRow, nbCols, nbAlts] = readInts sizes
      [nbCible, rayon, nbBloons, nbTurn] = readInts nbrs
      [rDep, cDep] = readInts depart
      cibles = map ((\[a, b] -> (a, b)) . readInts) . take nbCible $ datas
      mvts = readAlts nbRow . drop nbCible $ datas
  print (cibles, mvts)
  return ()

readInts :: String -> [Int]
readInts = map read . words

readAlts :: Int -> [String] -> [[[(Int, Int)]]]
readAlts nbRows datas = map readAlt $ splitEvery nbRows datas
  where readAlt l = map (map (\[a, b] -> (a, b)) . splitEvery 2 . readInts) $ l

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n l = d:splitEvery n f
  where (d, f) = splitAt n l
