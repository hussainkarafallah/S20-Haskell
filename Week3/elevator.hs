{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import CodeWorld
import CodeWorld.Image

-- I will start the code with generics

-- a function to find the next state to transition to by the action happened
-- we need an equality constraint on A
-- there is warning that patterns are not exauhstive
-- so the only possible solution was to switch this function to be specifically typed for State and make something like Crashed for failure detection
-- doing something like (Maybe b) won't work with interactionOf function so it's not a possibility, at least myself couldn't find a solution
-- The thing is the FSM should be well designed and everystate is covered with all actions so we shouldn't worry about this function
-- So if this function is used in real-life it's the user responsibility to ensure that his FSM is complete otherwise there won't be problems
findNextState :: Eq a => a -> [(a,s)] -> s
findNextState x ((y,z):rest) 
  | x == y = z
  | otherwise = findNextState x rest
  
-- a function that possibly takes an action and a current state and transition map function
-- and produces the next state
applyAction :: Eq a => Maybe a -> (s->[(a,s)]) -> s -> s
applyAction (Just action) transMap current = findNextState action (transMap current)
applyAction Nothing _ current = current



-- the complete elevator system
elevatorSystem
  :: Eq a -- must exist constraint replacing actions comparators i made them deriving Eq
  =>  s -- initial state of the fsm
  -> (s -> [(a, s)]) -- fsm transition map
  -> (Event -> Maybe a) -- the keylistener that converts keys to actions
  -> (s -> Picture) -- ^ drawing states
  -> system -- the system to be rendered. In our case we care about (elevatorstate,shuttleposition)
  -> (Double -> s -> system -> system) -- how system changes according to current state and time
  -> (system -> Picture) -- how to render the system
  -> IO ()

elevatorSystem initialState transMap listener drawState initialpos systemTimeSimulator renderSystem
  = interactionOf (initialState,initialpos) simulator eventHandler renderer
  where
    -- a function that changes the elevator system with time
    -- time only affects the shuttle position
    simulator t (state,shuttlePos) = (state, systemTimeSimulator t state shuttlePos)
  
    -- a function that changes the elevator system w.r.t key presses
    -- key presses restarts the timer and change only the state of the machine
    eventHandler event (state , shuttlePos) = (applyAction (listener event) transMap state , shuttlePos)
        
    -- a function that renders the whole elevator
    renderer (state,shuttlePos) = drawState state <> renderSystem shuttlePos



-- types of buttons
data Button = Up Bool | Down Bool | Stop Bool
  

-- a function to render each button
drawButton :: Button -> Picture

drawButton (Stop True) = rectangle 1 1 <> colored red (solidRectangle 0.5 0.5)
drawButton (Stop False) = rectangle 1 1 <> colored black (solidRectangle 0.5 0.5)

drawButton (Up True) = rectangle 1 1 <> translated (-0.37) (-0.32) (colored red (solidPolygon[(0,0),(0.8,0),(0.4,0.8)]))
drawButton (Up False) = rectangle 1 1 <> translated (-0.37) (-0.32) (colored black (solidPolygon[(0,0),(0.8,0),(0.4,0.8)]))

drawButton (Down True) = rectangle 1 1 <> translated (-0.37) (-0.4) (colored red (solidPolygon[(0,0.8),(0.8,0.8),(0.4,0)]))
drawButton (Down False) = rectangle 1 1 <> translated (-0.37) (-0.4) (colored black (solidPolygon[(0,0.8),(0.8,0.8),(0.4,0)]))

-- the function required in the document to draw spaced buttons
asSpaced :: Double -> (a->Picture) -> [a] -> Picture
asSpaced distance drawFunction pics
  | length pics == 0 = blank
  | otherwise = drawFunction (head pics) <> translated 0 (-distance) (asSpaced distance drawFunction (tail pics))

-- states of fsm
data State = MovingUp | MovingDown | Idle

-- possible actions
data Action = UpPressed | DownPressed | StopPressed
  deriving (Eq)

--transition map which is the same for every state
transitionMap :: s -> [(Action,State)]
transitionMap _ = [(UpPressed,MovingUp),(DownPressed,MovingDown),(StopPressed,Idle)]


-- function that listens to key strikes and gives the corresponding action
keyListener :: Event -> Maybe Action
keyListener (KeyPress "Up") = Just UpPressed 
keyListener (KeyPress "Down") =Just DownPressed 
keyListener (KeyPress " ") = Just StopPressed
keyListener _ = Nothing 



-- just a simple constant used for drawing
drawDis :: Double
drawDis = 1.0


-- a function to draw the button panel of the elevator
drawPanel :: State -> Picture
drawPanel  Idle = translated 5 0 (asSpaced drawDis drawButton [(Up False), (Stop True), (Down False)])
drawPanel  MovingUp =translated 5 0 ( asSpaced drawDis drawButton [(Up True),  (Stop False),(Down False)])
drawPanel MovingDown = translated 5 0 (asSpaced drawDis drawButton [(Up False), (Stop False) ,(Down True) ])



stonks :: Picture
stonks = image "stonks" "https://discordemoji.com/assets/emoji/6125_stonks.png" 3 3
shuttle :: Picture
shuttle = translated 0.7 (-0.5) stonks <> (rectangle 2.5 4)

elevatorHeight :: Double
elevatorHeight = 15.0

type ShuttlePosition = Double

-- a function to draw the elevator given the relative position of the shuttle
drawElevator :: ShuttlePosition -> Picture
drawElevator pos = translated 0 (-5) (translated 0 pos shuttle <> translated 0 (elevatorHeight / 2 - 2) (rectangle 3 elevatorHeight)) 

-- a function that gets the shuttle new position after a time t has elapsed since the last event
getShuttlePosition :: Double -> State -> ShuttlePosition -> ShuttlePosition
getShuttlePosition t MovingUp pos = min (pos + t) (elevatorHeight - 4)
getShuttlePosition t MovingDown pos = max (pos - t) 0
getShuttlePosition _ Idle pos =  pos
   
     
main :: IO()
main = elevatorSystem
  Idle
  transitionMap
  keyListener
  drawPanel
  0
  getShuttlePosition
  drawElevator