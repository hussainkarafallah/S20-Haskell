{-# LANGUAGE OverloadedStrings #-}

import CodeWorld


{-|
So basically the idea is to make separate lights each one with schedule setup
how long its cycle and at which time stamps should be on/off
Having a light which is a static picture and a schedule i have a function that produces an animation
each traffic light then is a collection of animations functioning together
I was able to combine them all with <> function
the main idea was to be able to build/modify traffic lights dynamically and add new componenets to them
without changing anything in the core code but rather creating new objects or defining completely new ones
-}

type Period = Integer --defines the length of 1 light cycle eg 8 seconds
type Stamps = [Integer] --a sequence for time stamps
type Frame = Picture -- frame for the traffic light
type Shape = Picture -- circle,square,rect
type Icon = Picture --- icon
type AnimatedLight = Double -> Picture ---a light that is setup for animation
type Range = (Double,Double) --- time range for blinking

--each light has a shape and color
data Light = Light Shape Color Icon
 

---a function that draws a light (on/off)
drawLight (Light pic color icon) x 
  | x == True = icon <> (colored color pic) 
  | x == False = (colored gray pic) 
  
  
---this presents the setup of the light (its period) when should be on/off etc and range for blinking
data LightSetup = LightSetup Period Stamps Range

---a function that takes a light and a timesystem and make the light work according to schedule
lightController :: Light -> LightSetup -> Double -> Picture

lightController light (LightSetup period stamps (blinkS , blinkE)) t
  | realMod >= blinkS && realMod <= blinkE = drawLight light (floor(t * 10) `mod` 2 == 0)
  | otherwise = (drawLight light (find (floor(t) `mod` period) stamps))
  where
    realMod = fromIntegral(floor(t) `mod` period) + (t - fromIntegral(floor(t))) 

---just to find an element inside a list
find _ [] = False
find x (y:xs)
  | x == y = True
  | otherwise = (find x xs)
  

---to combine a list of animations
combine t [] = blank
combine t (x : xs) = (x t) <> (combine t xs)
  
  
---each traffic light consists of 1 frame with multiple lights in it
data TrafficLight = TrafficLight Frame [AnimatedLight] 

trafficLightAnimation :: TrafficLight -> Double -> Picture 


---combine the frame with animations before rendering
trafficLightAnimation (TrafficLight frame lights) t
  = frame <> (combine t lights)
  

--- These are only declarations for objects


main :: IO()

carFrame = rectangle 2.5 7

carRLight = Light (translated 0 (2.2) (solidCircle 1)) red blank
carYLight = Light (translated 0 (0) (solidCircle 1)) yellow blank
carGLight = Light (translated 0 (-2.2) (solidCircle 1)) green blank

carRSetup = LightSetup 8 [0,1,2,3] (-1.0,-1.0)
carYSetup = LightSetup 8 [3,7] (-1.0,-1.0)
carGSetup = LightSetup 8 [4,5,6] (-1.0,-1.0)

carRController = lightController carRLight carRSetup
carYController = lightController carYLight carYSetup
carGController = lightController carGLight carGSetup

carLights = TrafficLight carFrame [carRController,carYController,carGController]
carLightsAnimation = trafficLightAnimation carLights


bikeFrame = translated 4 0 (rectangle 2.5 5)

bikeRLight = Light (translated 4 (1.2) (solidCircle 1)) red blank
bikeGLight = Light (translated 4 (-1.2) (solidCircle 1)) green (translated 4 (-1.2) (lettering "\x1F6B2"))

bikeRSetup = LightSetup 8 [0,1,2,3] (-1.0,-1.0)
bikeGSetup = LightSetup 8 [4,5,6] (7.0,8.0)

bikeRController = lightController bikeRLight bikeRSetup
bikeGController = lightController bikeGLight bikeGSetup

bikeLights = TrafficLight bikeFrame [bikeRController,bikeGController]
bikeLightsAnimation = trafficLightAnimation bikeLights

main = animationOf (bikeLightsAnimation <> carLightsAnimation) 