import qualified Euler3 as E3
import qualified Euler18 as E18

main :: IO ()
main = do
  print [E3.tailRec E3.n, E3.rec E3.n, E3.modular E3.n, E3.mapped E3.n, E3.comprehension E3.n, E3.infinite E3.n]
  print [E18.rec, E18.tailRec, E18.modular, E18.mapped, E18.comprehension, E18.infinite]
