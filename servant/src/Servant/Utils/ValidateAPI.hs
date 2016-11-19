{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Utils.ValidateAPI where

import GHC.TypeLits
import Servant.API

type family Flatten (prefix :: *) (api :: *) :: [*] where
  Flatten prefix (a :<|> b) = Flatten prefix a ++ Flatten prefix b
  Flatten prefix (part :> rest) = Flatten (Append prefix (part :> Nil)) rest
  Flatten prefix rest = '[Append prefix rest]

testFlatten = () ::
  Flatten Nil ("foo" :>
               (     "bar" :> Get '[] String
                :<|> "baz" :> Get '[] Int))
   ~ '[ "foo" :> "bar" :> Get '[] String
      , "foo" :> "baz" :> Get '[] Int
      ]

   => ()

data Nil

type family Append xs ys where
  Append Nil ys = ys
  Append (x :> xs) ys = x :> Append xs ys

type family MatchAny (pats :: [*]) (route :: *) :: Bool where
  MatchAny '[] route = False
  MatchAny (x ': xs) route = Match x route || MatchAny xs route

type family Match (pat :: *) (route :: *) :: Bool where
  Match ((segment :: Symbol) :> pat) (segment :> route) = Match pat route
  Match ((x       :: Symbol) :> pat) (y       :> route) = False
  Match (Capture a b :> pat) (Capture c d :> route) = Match pat route
  Match (Capture a b :> pat) ((c :: Symbol) :> route) = Match pat route
  Match (CaptureAll a b :> pat) c = True

  Match (pat1 :<|> pat2) route = Match pat1 route || Match pat2 route
  Match pat (route1 :<|> route2) = Match pat route1 || Match pat route2

  Match (a :> rest) b = False
  Match a (b :> rest) = False
  Match a b = True

type family a || b where
  True || b = True
  False || b = b

type family a && b where
  True && b = b
  False && b = False

type family Not b where
  Not True = False
  Not False = True

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family NonOverlapping' (with :: [*]) (routes :: [*]) :: Bool where
  NonOverlapping' with '[] = True
  NonOverlapping' with (x ': xs) = Not (MatchAny with x) && NonOverlapping' (with ++ '[x]) xs

type NonOverlapping api = NonOverlapping' '[] (Flatten Nil api)

testNonOverlapping1 = () :: NonOverlapping ("foo" :> (Get '[] Int :<|> Get '[] String)) ~ False => ()
testNonOverlapping2 = () :: NonOverlapping ("foo" :> (Get '[] Int :<|> "bar" :> Get '[] String)) ~ True => ()
