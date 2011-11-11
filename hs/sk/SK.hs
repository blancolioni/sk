module SK where

import Data.Array.IArray
import Data.Bits
import Data.Word

type Object = Word32
type Pair = Word64

memSize :: Object
memSize = 32768

data SKState = SKS { mem     :: Array Object Pair,
                     other   :: Array Object Pair,
                     top     :: Object,
                     last    :: Object
                   }

car, cdr :: SKState -> Object -> Object
car st addr = fromIntegral $ getPair st addr .&.
cdr st addr = fromIntegral $ getPair st addr  0x100000000
