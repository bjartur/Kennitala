module Kennitala (kennitala,gildKennitala,kennitöluHafi,vartölu,
                  dagur,mánuður,ár,raðtala,vartala,öld,
                  Aðili(Einstaklingur,Lögaðili),
                  mánaðardagar,hlaupár) where
import Text.Read
import Control.Applicative
import Data.Traversable

data Kennitala = Kennitala {
                   daggildi :: Int,
                   mánuður :: Int,
                   ár :: Int,
                   raðtala :: Int,
                   vartala :: Int,
                   öld :: Int
                 }

dagur :: Kennitala -> Int
dagur (Kennitala daggildi _ _ _ _ _) =
    if daggildi > 31
        then daggildi - 40
    else
        daggildi

data Aðili = Einstaklingur | Lögaðili deriving (Show, Eq)
kennitöluHafi :: Kennitala -> Aðili
kennitöluHafi kt = if dagur kt == daggildi kt
    then Einstaklingur
    else Lögaðili

-- Listi af tveimur seinustu stöfum tölu.
-- Nú eða núlli og tölunni, ef talan er minni en 10.
tveggjaStafaTala :: Int -> [Int]
tveggjaStafaTala t = [t `div` 10, t `mod` 10]

instance Show Kennitala where
    show (Kennitala daggildi mánuður ár raðtala vartala öld) =
        (show =<< tveggjaStafaTala  =<< [daggildi,mánuður,ár])  ++ "-" ++ (show =<< tveggjaStafaTala raðtala) ++ (show =<< [vartala,öld])
    -- 20 ≤ raðtala, svo fyrri tölustafurinn er aldrei núll => show raðtala == (show =<< tveggjaStafaTala raðtala)

isRight (Left  _) = False
isRight (Right _) = True

gildKennitala :: String -> Bool
gildKennitala = isRight . kennitala

breytaVillu :: (a -> b) -> Either a r -> Either b r
breytaVillu f = either (Left . f) Right

x & f = f x

krefjast :: (a -> Bool) -> String -> a -> Either String a
krefjast skilyrði melding x = if skilyrði x
    then Right x
    else Left melding

tala :: String -> (Int -> Bool) -> Char -> Char -> String -> Either String Int
tala heiti skilyrði a b melding = (readEither [a,b]
    & breytaVillu (const $ heiti ++ " má einungis innihalda tölustafi")
    >>= krefjast skilyrði melding)
    & breytaVillu (++ ", ekki " ++ [a,b] ++ ".")

tölustaf :: String -> Char -> Either String Int
tölustaf heiti a = let gildi = fromEnum a - fromEnum '0'
    in if   gildi > 9
       then Left $ heiti ++ " á að vera tölustafur, ekki " ++ [a] ++ "."
       else Right gildi

bæði :: (a -> Bool) -> (a -> Bool) -> a -> Bool
bæði = liftA2 (&&)

kennitala :: String -> Either String Kennitala
kennitala [dagtugur,dags,mántugur,mán,áratugur,árs,nr1,nr2,vartala,öld] =
    Kennitala <$>
    tala "Dagsetning" (0 <) dagtugur dags
        "Daggildi má mest vera 31 fyrir einstaklinga en minnst 41 fyrir félög. Daggildi má vera margt að 71" <*>
    tala "Mánuður" (bæði (0 <) (<= 12)) mántugur mán "Mánuðirnir eru tólf" <*>
    tala "Ártal" (0 <) áratugur árs "Ár má tákna með hvaða jákvæðu, tveggja stafa tölu sem er" <*>
    tala "Númer" (0 <=) nr1 nr2 "Raðtala er allt niður í 00, en oftast frá 20" <*>
    tölustaf "Vartala" vartala <*>
    tölustaf "Öld" öld

    >>= (\kt -> let dagar = mánaðardagar (ár kt) (mánuður kt)
         in krefjast (\kt -> dagur kt <= dagar)
         ("Dagar í " ++ show (mánuður kt) ++ ". mánuði eru " ++ show dagar ++ " talsins, ekki " ++ show (dagur kt))
         kt)

    >>= \kt ->
    vartölu [dagtugur,dags,mántugur,mán,áratugur,árs,nr1,nr2]
        >>= maybe (Left "Þessi kennitala getur ekki verið rétt. Hún á sér ekki vartölu.")
        (\útreiknuð ->
            if útreiknuð == vartala then Right kt
                else Left $ "Einhver fyrstu níu stafanna í kennitölunni er rangur. Níundi stafurinn ætti að vera " ++ [útreiknuð] ++ " en ekki  " ++ [vartala] ++ " ef fyrstu átta stafirnir eru rétir.")

-- Bandstrik má vera á milli 6. og 7. stafs
kennitala (dagtug:dag:mántug:mánuð:áratug:ár':'-':rest) = kennitala (dagtug:dag:mántug:mánuð:áratug:ár':rest)

kennitala _ = Left $ "Kennitala þarf að vera 10 tölustafir, að frátöldu valfrjálsu bandstriki."

-- Flestar 8 tölustafa runur eiga sér vartölu.
-- Stundum kemst út reiknuð vartala ekki fyrir í einum tölustaf.
-- Þá skilar eftirfarandi fall Nothing.
-- Það þýðir að kennitala getur ekki byrjað á tilteknu 8 tölustafa rununni.
-- Annaðhvort var runan rangt slegin inn, eða Þjóðskrá verður að velja nýtt númer.
-- Ef út reiknuð vartala kemst fyrir, þá skilar fallið Just henni.
vartöluFastar = 3:2:[7,6..2]
vartölu :: String -> Either String (Maybe Char)
vartölu strengur = if length strengur /= 8
    then Left "Vartala reiknast ekki nema af fyrstu 8 stöfum kennitölu."
    else do 
            listi <- traverse (tölustaf "Tölustafur") strengur :: Either String [Int]
            let niðurstaða = 11 - ((sum $ zipWith (*) vartöluFastar listi) `mod` 11)
            return . fmap (toEnum . (fromEnum '0' +)) $
                if niðurstaða == 11 then  Just 0
                else if niðurstaða == 10 then Nothing
                else Just niðurstaða

divisibleBy :: Int -> Int -> Bool
a `divisibleBy` b = a `rem` b == 0

implies :: Bool -> Bool -> Bool
imples x y = y || not x

hlaupár :: Int -> Bool
hlaupár ár = ár `divisibleBy` 4 && imples (ár `divisibleBy` 100) (ár `divisibleBy` 400)

mánaðardagar :: Int -> Int -> Int
mánaðardagar ár mánuður = case mánuður of
    02 -> 28 + fromEnum( hlaupár ár )
    04 -> 30
    06 -> 30
    08 -> 30
    11 -> 30
    _  -> 31
