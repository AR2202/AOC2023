{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}


module Day20 () where

import Control.Lens
import qualified Data.Map as M

data PulseType = High | Low deriving (Show, Read, Eq)

data Switch = On | Off deriving (Show, Read, Eq)

type Connections = [String]

type Module = Either FlipFlop Conjunction

type PulseMap = M.Map String PulseType

data FlipFlop = FlipFlop {_nameF :: String, _state :: Switch, _connections :: Connections} deriving (Show, Read, Eq)

data Conjunction = Conjunction {_nameC :: String, _inputs :: PulseMap, _outputs :: Connections} deriving (Show, Read, Eq)

makeLenses ''Conjunction
makeLenses ''FlipFlop

-- Part 1
--day20a :: (Module, Maybe PulseType)
--day20a = receive (Left (FlipFlop Off ["b"])) Low "broadcaster"

switch :: Switch -> Switch
switch On = Off
switch Off = On

switchPulse :: Switch -> PulseType
switchPulse On = High
switchPulse Off = Low

switchState :: FlipFlop -> FlipFlop
-- switchState ff = ff {_state = switch (_state ff)}
switchState = over state switch

receive :: Module -> PulseType -> String -> (Module, Maybe PulseType)
receive (Left flipFlop) High _ = (Left flipFlop, Nothing)
receive (Left flipFlop) Low _ = (Left (switchState flipFlop), Just (send (Left (switchState flipFlop))))
receive (Right conjunction) pulsetype str = (Right (over inputs (M.insert str pulsetype) conjunction), Just (send (Right (over inputs (M.insert str pulsetype) conjunction))))

send :: Module -> PulseType
send (Left flipflop) = switchPulse (_state flipflop)
send (Right conjunction) = if all (== High) (M.elems (_inputs conjunction)) then Low else High

extractOutputs (Left flipflop) = _connections flipflop
extractOutputs (Right conjunction) = _outputs conjunction

extractName (Left flipflop) = _nameF flipflop
extractName (Right conjunction) = _nameC conjunction

handlePulses [] pulsemap = pulsemap
handlePulses ((m, Nothing) : pulses) pulsemap = handlePulses pulses pulsemap

--handlePulses ((m, Just pulse) : pulses) pulsemap
