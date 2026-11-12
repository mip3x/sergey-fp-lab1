import qualified Euler3 as E3
import qualified Euler18 as E18
import System.Exit (exitFailure)

variants3 :: [Integer -> Integer]
variants3 = [E3.tailRec, E3.rec, E3.modular, E3.mapped, E3.comprehension, E3.infinite]

variants18 :: [Int]
variants18 = [E18.rec, E18.tailRec, E18.modular, E18.mapped, E18.comprehension, E18.infinite]

known3 :: [(Integer, Integer)]
known3 = [(2, 2), (3, 3), (4, 2), (12, 3), (97, 97), (6857, 6857), (13195, 29)]

main :: IO ()
main = do
  let wrongKnown = [(n, i) | (n, e) <- known3, (i, f) <- zip [0 :: Int ..] variants3, f n /= e]
      disagree = [n | n <- [2 .. 2000], let rs = map ($ n) variants3, any (/= head rs) rs]
      wrong18 = [i | (i, v) <- zip [0 :: Int ..] variants18, v /= 1074]
  mapM_ (putStrLn . ("задача 3, неверный ответ (n, вариант): " ++) . show) wrongKnown
  mapM_ (putStrLn . ("задача 3, варианты расходятся при n = " ++) . show) disagree
  mapM_ (putStrLn . ("задача 18, неверный вариант: " ++) . show) wrong18
  if null wrongKnown && null disagree && null wrong18
    then putStrLn "все тесты пройдены"
    else exitFailure
