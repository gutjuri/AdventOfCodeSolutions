import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06

run :: Int -> IO () -> IO ()
run dayNr func = do
    putStrLn ""
    putStrLn $ "==== " ++ show dayNr ++ " ===="
    func

main :: IO ()
main = do
    run 1 Day01.main
    run 2 Day02.main
    run 3 Day03.main
    run 4 Day04.main
    run 5 Day05.main
    run 6 Day06.main