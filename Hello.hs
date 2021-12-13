module Hello where
import Text.XHtml (content)
import Html


myHtml = html_ "my Title" "Stfu"

main :: IO ()
main = putStrLn (render myHtml)