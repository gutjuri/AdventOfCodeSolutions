import Data.Array (Array)
import qualified Data.Array as A

type Grid = Array (Int, Int) Bool
type Instr = [()]

exec :: Instr -> Grid -> Grid
exec [] g     = g
exec (i:ix) g = fmap 

main = do
  

