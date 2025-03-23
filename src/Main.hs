import Language.Joy.Rewrite

import Control.Exception (finally)
import Data.Text (Text)
import System.Directory (renameFile)
import System.Environment (getArgs)
import System.IO
import System.IO.Temp (withSystemTempFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    rules <- T.readFile "hello_rules.txt"
    [fileName] <- getArgs
    contents <- T.readFile fileName
    case rewrite (T.lines rules) (T.lines contents !! 0) of
        Left _ -> error "no parse"
        Right xs ->
              withSystemTempFile "temp.txt" $ \tempPath tempHandle -> do
                T.hPutStr tempHandle (T.unwords xs)
                hClose tempHandle
                renameFile tempPath fileName
