import qualified Day01
import qualified Day02
import qualified Day03

run :: Int -> IO () -> IO ()
run dayNr func = do
    putStrLn ""
    putStrLn $ "==== " ++ show dayNr ++ " ===="
    func

main = do
    -- Day01.main
    run 2 Day02.main
    run 3 Day03.main