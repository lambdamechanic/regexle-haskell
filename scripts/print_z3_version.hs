#!/usr/bin/env runghc
import Z3.Monad

main :: IO ()
main = do
  Version major minor build revision <- evalZ3 getVersion
  putStrLn $ show major ++ "." ++ show minor ++ "." ++ show build ++ " (rev " ++ show revision ++ ")"
