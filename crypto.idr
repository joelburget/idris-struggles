module Crypto

import Data.Bits

%default total

ntb : (n : Nat) -> (lt n 16 = True) -> Bits8
ntb n p = natToBits n

hexVect : Vect 16 Char
hexVect = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
           'A', 'B', 'C', 'D', 'E', 'F']

switchMaybe : List (Maybe a) -> Maybe (List a)
switchMaybe [] = Just []
switchMaybe (Nothing::_) = Nothing
switchMaybe (Just a::ms) = case switchMaybe ms of
    Nothing => Nothing
    Just as => Just (a::as)

parseHex : String -> Maybe (List Nat)
parseHex str = result where
  lst : List Char
  lst = unpack str
  ms : List (Maybe Nat)
  ms = map (\c => findIndex (== c) hexVect) lst
  conv : Nat -> Bits8
  conv n = ntb n ?help
  bs : List (Maybe Bits8)
  bs = map (map conv) ms
  result = switchMaybe ms
parseB64 : String -> List Bits8

printHex : List Bits8 -> String
-- printHex = pack . map show

printB64 : List Bits8 -> String
