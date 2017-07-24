
data Sysout a = ResOut a String
     deriving (Show)

(ResOut res1 out1) >>= f = let (ResOut res2 out2) = f res1
                           in (ResOut res2 (out1 ++ " " ++ out2))

return x = ResOut x ""


numberAndText = ResOut 7 "Hallo Herbstcampus!"

calcAndText x = ResOut (4 + x) "Ich kann rechnen!"

main = do {
     print (numberAndText Main.>>= calcAndText);
}
