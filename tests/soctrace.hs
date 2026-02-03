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
        romName = printf "test-data/%s_rom.bin.txt" tname
        vcdName = printf "test-data/%s_trace.vcd" tname
        soc = exposeClockResetEnable (let ?doTrace = True in simpleSoc romName)
            systemClockGen
            systemResetGen
            enableGen
    result <- dumpVCD (0, ncycles) soc ["PC", "mem_in", "reg_read_addr"]
    case result of
        Left msg -> error msg
        Right contents -> T.writeFile vcdName contents
    return ()