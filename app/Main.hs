import MainTool.MainTool
import MainConsole.MainConsole

{-  Commands for compiling and running the program:

    Compiling:
    ghc -O -rtsopts -o haskellApp Main.hs -package base -package time -package bytestring -package vector -package filepath -package directory -package split -package yesod -package yesod-form -package blaze-markup -package warp -package monad-logger

    Running: 
    ./haskellApp +RTS -sstderr

-}

main :: IO ()
main = do
    putStrLn "Please select either tool or console (without \"\")."
    selectedUI <- getLine
    callUI selectedUI


callUI :: String -> IO()
callUI "tool" = mainApp
callUI _ = mainConsole
