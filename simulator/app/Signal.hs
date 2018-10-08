{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Signal where

import Control.Applicative        (liftA2, liftA3)
import Control.DeepSeq            (NFData, force)
import Control.Exception          (catch, evaluate, throw)
import Data.Default               (Default (..))
import GHC.Generics               (Generic)
import GHC.Stack                  (HasCallStack, withFrozenCallStack)
import GHC.TypeLits               (KnownNat, KnownSymbol, Nat, Symbol)
import Language.Haskell.TH.Syntax (Lift (..))
import System.IO.Unsafe           (unsafeDupablePerformIO)
data Domain = Dom { domainName :: Symbol, clkPeriod :: Nat }

infixr 5 :-

data Signal (domain :: Domain) a
  = a :- Signal domain a

instance Show a => Show (Signal domain a) where
  show (x :- xs) = show x ++ " " ++ show xs

instance Lift a => Lift (Signal domain a) where
  lift ~(x :- _) = [| signal# x |]

instance Default a => Default (Signal domain a) where
  def = signal# def

instance Functor (Signal domain) where
  fmap = mapSignal#

{-# NOINLINE mapSignal# #-}
mapSignal# :: (a -> b) -> Signal domain a -> Signal domain b
mapSignal# f (a :- as) = f a :- mapSignal# f as

instance Applicative (Signal domain) where
  pure  = signal#
  (<*>) = appSignal#

{-# NOINLINE signal# #-}
signal# :: a -> Signal domain a
signal# a = let s = a :- s in s

{-# NOINLINE appSignal# #-}
appSignal# :: Signal domain (a -> b) -> Signal domain a -> Signal domain b
appSignal# (f :- fs) xs@(~(a :- as)) = f a :- (xs `seq` appSignal# fs as)