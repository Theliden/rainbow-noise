module PPMOutput where

import qualified Data.ByteString.Lazy as BL
import Data.Word

output :: Int -> Int -> [Data.Word.Word8] -> IO ()
output height width bytes = do
  putStrLn $ "P6 " ++ show width ++ " " ++ show height ++ " 255"
  BL.putStr (BL.pack bytes)
