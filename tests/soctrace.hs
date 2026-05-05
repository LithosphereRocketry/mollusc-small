import Text.Printf
import System.Environment
import Clash.Prelude hiding (writeFile, dumpVCD, traceSignal)
import SOC.SimpleSOC
import qualified Data.Text.IO as T
import qualified Data.Aeson as J

import Clash.Shockwaves.Trace

main :: IO ()
main = do
    args <- getArgs
    let [tname, ncycles_txt] = args
        (ncycles :: Int) = read ncycles_txt
        romName = printf "test-data/%s_rom.bin.txt" tname
        vcdName = printf "test-data/%s_trace.vcd" tname
        metaName = printf "test-data/%s_trace.json" tname
        soc = exposeClockResetEnable (let ?doTrace = True in simpleSoc romName)
            systemClockGen
            systemResetGen
            enableGen
    result <- dumpVCD (0, ncycles) soc ["PC", "mem_in", "reg_read_addr"]
    case result of
        Left msg -> error msg
        Right (vcd,meta) -> do
            T.writeFile vcdName vcd
            J.encodeFile metaName meta
    return ()