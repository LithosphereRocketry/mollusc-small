import qualified Prelude as P
import Text.Printf
import System.Environment
import Clash.Prelude
import SOC.SimpleSOC
import qualified Data.Text.IO as T

main :: IO ()
main = do
    args <- getArgs
    let tname = P.head args
        (ncycles :: Int) = read $ P.head $ P.tail args
        romName = printf "test-data/%s_rom.bin" tname
        vcdName = printf "test-data/%s_trace.vcd" tname
        soc = exposeClockResetEnable (simpleSoc romName)
            systemClockGen
            systemResetGen
            enableGen
    print ncycles 
    result <- dumpVCD (0, ncycles) soc ["PC", "IR"]
    case result of
        Left msg -> error msg
        Right contents -> T.writeFile vcdName contents
    return ()