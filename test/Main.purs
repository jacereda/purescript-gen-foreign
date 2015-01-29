module Test.Main where

import Debug.Trace
import GenForeign
import Data.String

synonyms = [ S "Synonym" "Number"
           ]
           
enums = [ E "Enumerated" [ EE "NEOpt1" "default"
                         , EE "NEOpt2" "other"
                         ]
        ]
        
alts = [ A "Alt" [ AA "Alt1" "Number" "isNumber"
                 , AA "Alt2" "auto" "isString"
                 , AA "Alt3" "[Number]" "isArray"
                 ]
       ]
       
records = [ R "Rec" [ RR "a" "String" "\"default\""
                    , RR "b" "Number" "0"
                    , RR "c" "Maybe [Maybe Number]" "Nothing"
                    ]
          ]

main = do
    trace $ joinWith "\n" $ dumpH <> (dumpS <$> synonyms) <> (dumpE <$> enums) <> (dumpT <$> records) <> (dumpA <$> alts)
    
