{-# LANGUAGE QuasiQuotes #-}
module Arduino (PinType(..), setupPins) where

import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax as C
import Language.C.Quote.C

data PinType = Input | Output

instance ToIdent PinType where
    toIdent Input = toIdent "INPUT"
    toIdent Output = toIdent "OUTPUT"

setupPins :: String
          -> [(String, Int, PinType, String)]
          -> ([C.Definition], [C.Definition], [String])
setupPins atomName pins = (decls, defs, pinFuncNames)
  where
    decls = [cunit|

             // OH GOD WHY?!
             $esc:("#include <Arduino.h>")

             $edecls:pinDecls
             $edecls:pinFuncDecls
            |]

    defs = [cunit|
            $edecl:setup
            $edecls:pinFuncDefs
            $edecl:loop
           |]

    pinDecls = [ [cedecl| typename uint16_t $id:pinName = $int:pinNum; |]
               | (pinName, pinNum, _, _) <- pins
               ]

    pinFuncDecls = [ [cedecl| void $id:(toPinFunc pinName)(); |]
                   | (pinName, _, pinType, _) <- pins
                   ]

    pinInit = [ [cstm| pinMode($id:pinName, $id:pinType); |]
              | (pinName, _, pinType, _) <- pins
              ]

    pinFuncDefs = [ [cedecl| void $id:(toPinFunc pinName) () { $stm:updatePin } |]
                  | (pinName, _, pinType, stateVar) <- pins
                  , let updatePin = case pinType of
                             Input -> [cstm| state.$id:atomName.$id:stateVar = digitalRead($id:pinName); |]
                             Output -> [cstm| digitalWrite($id:pinName, state.$id:atomName.$id:stateVar); |]
                  ]

    setup = [cedecl| void setup () { $stms:pinInit } |]
    loop = [cedecl| void loop () { $id:atomName (); } |]

    pinFuncNames = [ toPinFunc pinName | (pinName, _, _, _) <- pins ]

    toPinFunc pinName = "update_" ++ pinName

