import EPseudocode.Data as EPD
import EPseudocode.Parser as EPP

main :: IO ()
main = EPP.runLex EPP.mainParser "asd"
