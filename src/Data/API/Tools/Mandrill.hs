{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.API.Tools.Mandrill
       ( mandrillTool ) where

import Network.API.Mandrill.Response
import Control.Monad (when)
import Data.API.JSON
import Data.API.Tools.Combinators
import Data.API.Tools.Datatypes
import Data.API.Types
import Data.Monoid
import Language.Haskell.TH
import Data.Default

mandrillTool :: APITool
mandrillTool = apiNodeTool $ apiSpecTool
  gen_sn_ex    
  gen_sr_ex    
  gen_su_ex    
  gen_se_ex    
  mempty

gen_sr_ex :: Tool (APINode, SpecRecord)
gen_sr_ex = mkTool $ \ ts (an, _) -> mkInst ts an (expCon an) (expLCon an)

gen_sn_ex :: Tool (APINode, SpecNewtype)
gen_sn_ex = mkTool $ \ ts (an, _) -> mkInst ts an (expCon an) (expLCon an)            

gen_su_ex :: Tool (APINode, SpecUnion)
gen_su_ex = mkTool $ \ ts (an, _) -> mkInst ts an (expCon an) (expLCon an)

gen_se_ex :: Tool (APINode, SpecEnum)
gen_se_ex= mkTool $ \ ts (an, _) -> mkInst ts an (expCon an) (expLCon an)

mkInst :: ToolSettings -> APINode -> ExpQ -> ExpQ -> Q [Dec]
mkInst ts an e1 e2 =
  optionalInstanceD ts
    ''MandrillResponse
    [nodeRepT an]
    (simpleD 'parseResponse e1)
    (simpleD 'parseResponse e2)

optionalInstanceD :: ToolSettings -> Name -> [TypeQ] -> DecQ -> DecQ -> Q [Dec]
optionalInstanceD stgs c tqs dq1 dq2 = do
  ts <- sequence tqs
  ds1 <- dq1
  ds2 <- dq2
  exists <- isInstance c ts
  if exists then do
    when (warnOnOmittedInstance stgs) $ reportWarning $ msg ts
    return []
  else 
    return [
        InstanceD [] (AppT (ConT c) (head ts)) [ds1],
        InstanceD [] (AppT (ConT c) (AppT ListT (last ts))) [ds2]
      ]
  where
  msg ts = "instance " ++ pprint c ++ " " ++ pprint ts ++ " already exists, so it was not generated"
	
simpleD :: Name -> ExpQ -> Q Dec
simpleD n e = funD n [clause [] (normalB e) []]

expCon :: APINode -> ExpQ
expCon an = [e| \resp ->
  case decodeWithErrs resp :: Either [(JSONError, Position)] $(nodeRepT an) of
   Right u -> Right u
   Left  p ->
     case decodeWithErrs resp :: Either [(JSONError, Position)] ApiError of
      Right e -> Left e
      Left  pp -> Left def { message = "first: " ++ show p ++ " next: " ++ show pp}
  |]

expLCon :: APINode -> ExpQ
expLCon an = [e| \resp ->
  case decodeWithErrs resp :: Either [(JSONError, Position)] [$(nodeRepT an)] of
   Right u -> Right u
   Left  p ->
     case decodeWithErrs resp :: Either [(JSONError, Position)] ApiError of
      Right e -> Left e
      Left  pp -> Left def { message = "first: " ++ show p ++ " next: " ++ show pp}
  |]
