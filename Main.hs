{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Lens
import Data.List
import Data.Function
import Text.Printf
import Data.IORef
import Control.Monad
import Data.Array
import Data.Array.IO
import System.Random
import Data.Maybe

n :: Num a => a -> a
n = negate
r :: Fractional a => a -> a
r = recip

data Farbe = Oichl | Gros | Heaz | Schain deriving (Show, Eq, Enum, Bounded)
data Wert = Oss | King | Oba | Unta | Zehner | Neiner | Ochta | Simma deriving (Show, Eq, Enum, Bounded)

data Entscheidung = Ziehen | Halten deriving (Show, Eq, Enum)

data Karte = Karte { _farbe :: Farbe, _wert :: Wert } deriving (Show, Eq)
makeLenses ''Karte

data Spieler = Spieler { _karten :: [Karte], _summe :: Int, _lebendig :: Bool , _name :: String, _gehalten :: Int, _darfHalten :: Bool } deriving (Show, Eq)
makeLenses ''Spieler

data Stapel = Stapel { _kartenStapel :: [Karte] } deriving (Show, Eq)
makeLenses ''Stapel

data SpuiZuastond = SpuiZuastond
  { _spielStapel :: Stapel
  , _spielerArray :: Array Int (IORef Spieler)
  , _anzahlSpieler :: Int
  , _uebrigeSpieler :: Int
  , _indexSpieler :: Int
  , _nexteKarte :: Maybe Karte
  }
makeLenses ''SpuiZuastond

kartenWert :: Karte -> Int
kartenWert (Karte _ w) = case w of
  Oss -> 11
  King -> 4
  Oba -> 3
  Unta -> 2
  Zehner -> 10
  Neiner -> 9
  Ochta -> 8
  Simma -> 7

stapelNichtLeer x = not $ null $ x ^. kartenStapel

aktion :: Entscheidung -> (Stapel, Spieler) -> (Stapel, Spieler, Maybe Karte)
aktion Halten (a, b) = (a, b, Nothing)
aktion Ziehen (stapel, spieler) = case karte of
  Nothing -> (stapel, spieler, Nothing)
  Just karte -> (neuerStapel
                , spieler & karten %~ (karte :) & summe +~ kartenWert karte
                , Just karte)
  where
    (karte, neuerStapel) = zieheStapel stapel

zieheStapel (Stapel xs) = (safeHead xs, Stapel $ tail xs)

stapel = [Karte farbe wert | farbe <- [minBound..maxBound], wert <- [minBound..maxBound]]

safeHead [] = Nothing
safeHead (x:_) = Just x

erstelleSpieler :: String -> Spieler
erstelleSpieler x = Spieler [] 0 True x 0 True

magus = erstelleSpieler "König von Bayern"
fritze = erstelleSpieler "König von Preußen"
menschenfreund = erstelleSpieler "Des war mein Bruder"
kobold = erstelleSpieler "Grundschauen schulen"
schtanzy = erstelleSpieler "Fancy Nancy"

spielSpieler = [magus, fritze, menschenfreund, kobold, schtanzy]

-- des ged bessa
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

zuastond = do
  spuiler <- traverse newIORef spielSpieler
  gemischterStapel <- Stapel <$> shuffle stapel
  let
    l = length spuiler
    (karte, stapel') = zieheStapel gemischterStapel

  pure $ SpuiZuastond
    stapel'
    (listArray (0, l) spuiler)
    l
    l
    0
    karte

main :: IO ()
main = do
  z <- zuastond
  spielen z

spielen spielZustand = do
  let
    sp = (spielZustand ^. spielerArray) ! (spielZustand ^. indexSpieler)
    naechsterIndex = mod (spielZustand ^. indexSpieler + 1) (spielZustand ^. anzahlSpieler)
  spieler <- readIORef sp
  let
    s = spieler ^. summe
    format :: String
    format = printf "/home/\"%s\"/> " (spieler ^. name)
    ele = elems $ spielZustand ^. spielerArray

  case spielZustand ^. nexteKarte of
    Nothing -> do
      z <- head . sortBy (on compare _summe) <$> traverse (readIORef) ele
      printf "Der Stapel ist leer, %s hat mit einer Summe von %d gewonnen\n" (z ^. name) (z ^. summe)
      pure ()
    _ | spielZustand ^. uebrigeSpieler == 1 ->
        if | spieler ^. lebendig -> do
                printf "Alle anderen wurden vernichtet\n"
                printf "Herzlichen Glückwunsch zum Sieg, %s\n" (spieler ^. name)
           | True -> spielen $ spielZustand & indexSpieler .~ naechsterIndex
    Just karte | not (spieler ^. lebendig) ->
                 spielen $ spielZustand & indexSpieler .~ naechsterIndex
    Just karte | spieler ^. lebendig -> do
      printf "%sPunktestand: %d.\nZiehen? [y/n] " format s
      antwort <- zuEntscheidung <$> getLine

      neuerSpieler <- case antwort of
        Nothing -> do
          printf "%sUngültige Eingabe\n" format
          pure spieler
        Just Halten -> do
          printf "Alles bleibt beim Alten\n"
          pure spieler
        Just Ziehen -> do
          printf "%sGezogene Karte: %s %s ⇒ Punktzahl +%d\n" format (show $ karte ^. farbe) (show $ karte ^. wert) (kartenWert karte)
          pure $ spieler
            & karten %~ (karte :)
            & summe +~ kartenWert karte
            & lebendig .~ (kartenWert karte + s <= 21)
      writeIORef sp neuerSpieler
      let
        s = neuerSpieler ^. summe
        (neueKarte, neuerStapel) = zieheStapel $ spielZustand ^. spielStapel
        nexterZustand = spielZustand
          & spielStapel .~ neuerStapel
          & nexteKarte .~ neueKarte
          & indexSpieler %~ (\x -> mod (x + 1) (spielZustand ^. anzahlSpieler))
      if | s == 21 -> do
             printf "%sNeuer Punktestand: %d\n" format s
             printf "%s hat gewonnen\n" $ neuerSpieler ^. name
         | s > 21 -> do
             printf "%sPunktestand überschreitet 21. Get rekt du kek.\n\n" format
             spielen $ nexterZustand & uebrigeSpieler +~ (n 1)
         | True -> do
           printf "%sPunktestand: %d\n\n" format s
           spielen nexterZustand

zuEntscheidung :: String -> Maybe Entscheidung
zuEntscheidung = \case
    ('y':_) -> Just Ziehen
    ('n':_) -> Just Halten
    _ -> Nothing
