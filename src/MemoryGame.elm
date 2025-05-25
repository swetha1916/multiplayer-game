module MemoryGame exposing (..)

import Random exposing (..)
--import Random.List
import Tuple
import Set

--shuffleCards list seed = Random.step (Random.List.shuffle list) seed

indexGenerator = (int 0 3)

getSeed model = initialSeed (round <| model.time * 1000)

getIndex seed = 
  let (index, seeds) = Random.step (indexGenerator) seed
  in
  index

-- Get a passage based on index
getPassage passages i =
  case passages of
    (x :: xs) -> if i == 0 then x
                 else getPassage xs (i - 1)
    _ -> []

myShapes model =
  (
  case model.currentstate of 
    Game ->   
          [ 
            square 500 
              |> filled (hsl (degrees 217) 0.359 0.275)
            , snowflakes
            , text "Memory Game"
               |> centered
               |> bold
               |> filled white
               |> addOutline (solid 0.2) (hsl (degrees 217) 0.383 0.158)
               |> move (0,50)
               |> scale 1
            , rect 175 40
               |> filled (hsl (degrees 217) 0.383 0.158)
               |> addOutline (dotted 1) white
               |> move (-1.7,5)
                
          ]
          ++
          (
          let
            (flippedSet,flipAngle)
              = case model.state of
                  OneFlipped (idx1,_)              -> ([idx1]      |> Set.fromList, timeToShow)
                  TwoFlipped (idx1,_) (idx2,_) ang -> ([idx1,idx2] |> Set.fromList, ang)
                  Waiting                          -> (Set.empty,                   timeToShow) 
          in 
            model.cards 
              |> List.map
                  ( \ (idx,c) -> 
                        if Set.member idx flippedSet then
                          reconvert (Debug.toString c)
                            |> scaleX ( flipAngle / timeToShow )
                            |> move ( -75 + 21 * toFloat idx, 5)
                        else
                          [ 
                           roundedRect 18 25 2
                            |> filled blue
                            |> addOutline (solid 1) (hsl (degrees 188) 0.874 0.748)
                            |> move ( -75 + 21* toFloat idx, 5)
                          , snowflake (hsl (degrees 188) 0.874 0.748) white |> scale 0.12 |> move ( -71 + 21 * toFloat idx, 5)
                          ] |> group |> notifyTap (Select (idx,c))
                  )
           )
           ++
           [  score model, exitButton
            -- transition between the end screen and the game screen
            , if (List.isEmpty model.cards) then
                 [
                   roundedRect 59 25 2
                    |> filled blue
                    |> addOutline (solid 1) (hsl (degrees 188) 0.874 0.748)
                    |> move (0,3)
                  ,
                  text ("You win " ++ (String.fromInt (model.score)) ++ " coins!")
                    |> filled (hsl (degrees 188) 0.874 0.748)
                    |> scale 0.6
                    |> move (-26,7)
                  , 
                  
                  roundedRect 35 10 2
                    |> filled (hsl (degrees 217) 0.383 0.158)
                    |> addOutline (dotted 1) white
                    |> move (-1,-1)
                  ,
                  text "Play Again?"
                    |> filled (hsl (degrees 188) 0.874 0.748)
                    |> scale 0.4
                    |> move (-12,-2.5)
                ] |> group |> move (0,2)
                  |> notifyTap Play
              else
                [
                ] |> group
           ]
    End -> 
           [ square 500 
              |> filled (hsl (degrees 217) 0.359 0.275)
           , snowflakes
           , 
           [ roundedRect 50 10 2
                    |> filled (hsl (degrees 217) 0.383 0.158)
                    |> addOutline (dotted 1) white
                    |> move (-1,-1)
                  ,
             text "Click to play again :)"
                    |> filled (hsl (degrees 188) 0.874 0.748)
                    |> scale 0.4
                    |> move (-22,-2.5) ]
             |> group |> move (0,2) |> scale 2 |> notifyTap Play
            ]
               
             -- game is reinitialised
             -- the game has to be re stated to import the libraries 
           
     
     
   )
     
  
-- used to convert the integers to call the sketch of the shapes 
reconvert x = 
    case x of
        "1" -> hearts
        "2" -> clubs
        "3" -> spades
        _ -> diamond
        
          
type Msg = Tick Float GetKeyState
         | Select (Int,Int) | Play | Exit

type State = Waiting
           | OneFlipped (Int,Int)
           | TwoFlipped (Int,Int) (Int,Int) CountDown
           
type CurrentState = Game | End

type alias CountDown = Float
timeToShow = 1 -- second

type alias Model = { time : Float
                   , cards : List Int
                   , state : State
                   , currentstate: CurrentState
                   , check : Bool
                   , score : Int
                   , exit : Bool
                   , coins : Int
                   , startTime : Float
                   }

update msg model = case msg of
                     Tick t _ -> 
                       case model.state of
                         TwoFlipped card1 card2 countDown ->
                           if countDown <= 0 then
                             if Tuple.second card1 == Tuple.second card2 then
                               { model | cards = model.cards
                                                   |> List.filter 
                                                        ( \ card -> card /= card1 && card /= card2 )
                                       , state = Waiting
                                       , score = model.score + 2
                                       , time = t
                                       }
                             else 
                               { model | state = Waiting
                                       , time = t
                                       }
                           else 
                             let 
                               dt = t - model.time
                             in
                               { model | time = t
                                       , state = TwoFlipped card1 card2 (countDown - dt)
                                       }
                         _ -> 
                           { model | time = t }
                     
                     Select card ->
                       case model.state of 
                         Waiting -> { model | state = OneFlipped card}
                         OneFlipped card1 -> { model | state = TwoFlipped card1 card timeToShow}
                         _ -> model
                     Exit -> { model | exit = True
                                     , coins = (if (List.isEmpty model.cards) then model.coins + model.score
                                               else 0) }
                     Play -> 
                             if (model.currentstate == Game) then
                               {model | currentstate = End}
                             else 
                               let 
                                  --(newCards, newSeed) = shuffleCards [1,1,2,2,3,3,4,4] (Random.initialSeed 7)
                                  newCards = getPassage [cards1,cards2,cards3,cards4] (getIndex (Random.initialSeed (round model.time)))
                                  newCoins = model.coins + 8
                               in
                                 { model | time = 0
                                        , cards = List.indexedMap ( \ idx c -> (idx,c) ) newCards
                                        , state = Waiting
                                        , currentstate = Game
                                        , check = False
                                        , score = 0
                                        , coins = newCoins
                                        , startTime = 0.0
                                 }

init = 
  let 
    --(newCards, newSeed) = shuffleCards [1,1,2,2,3,3,4,4] (Random.initialSeed 7)
    --newCards = [1,1,2,2,3,3,4,4]
      newCards = getPassage [cards1,cards2,cards3,cards4] (getIndex (Random.initialSeed 7))
  in
    { time = 0
    , cards = List.indexedMap ( \ idx c -> (idx,c) ) newCards
    , state = Waiting
    , currentstate = Game
    , check = False
    , score = 0
    , exit = False
    , coins = 0
    , startTime = 0.0
    }
    
    
main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)
-----------------------------------------------------------------------------------
clubs = 
    [ roundedRect 14 20 2 
        |> filled white
        |> addOutline (solid 2) black
    , circle 10
        |> filled black
        |> scale 0.25
        |> move (0,3)
    , circle 10
        |> filled black
        |> scale 0.25
        |> move (2,0)
    , circle 10
        |> filled black
        |> scale 0.25
        |> move (-2,0)
    , circle 10
        |> filled black
        |> scale 0.25
        |> move (0,0)
    , triangle 4
        |> filled black
        |> scale 0.55
        |> rotate (degrees 90)
        |> move (0,-3)
    ] |> group
    
hearts = 
    [ roundedRect 14 20 2 
        |> filled white
        |> addOutline (solid 1) red
        |> scale 4
        |> move (-32,30)
    , curve (-32.07,46.313) [Pull (-27.33,54.431) (-17.98,52.908),Pull (-11.18,51.901) (-9.592,44.814),Pull (-8.440,37.770) (-13.48,30.725),Pull (-20.09,22.152) (-32.37,15.138),Pull (-44.14,22.150) (-53.35,34.323),Pull (-56.64,41.817) (-53.05,49.311),Pull (-49.42,53.949) (-44.06,53.508),Pull (-35.80,53.131) (-32.07,46.313)]
        |> filled red
        |> addOutline (solid 2) black
        |> move (0,-3)
    ] |> group |> scale 0.23 |> move (7,-7)
      
      
spades =
    [ roundedRect 14 20 2 
        |> filled white
        |> addOutline (solid 1) black
        |> scale 2.7
        |> move (6,0)
    , curve (5.9953,16.337) [Pull (3.7774,13.339) (0.5995,10.341),Pull (-2.548,7.4941) (-5.695,4.6463),Pull (-7.244,2.5480) (-7.194,0.4496),Pull (-7.325,-1.798) (-5.096,-4.046),Pull (-2.728,-5.816) (0,-6.145),Pull (2.0983,-6.345) (4.1967,-4.346),Pull (4.1670,-6.145) (3.2974,-7.943),Pull (2.3185,-9.742) (0.2997,-11.54),Pull (5.6955,-11.54) (11.091,-11.54),Pull (9.2823,-9.742) (8.3934,-7.943),Pull (7.3237,-6.145) (7.4941,-4.346),Pull (9.4426,-5.856) (11.391,-5.845),Pull (13.789,-6.265) (16.187,-4.646),Pull (18.346,-2.977) (18.585,-0.749),Pull (19.115,1.3489) (18.285,3.4473),Pull (17.007,5.5456) (14.088,7.6440),Pull (11.840,9.5925) (9.5925,11.540),Pull (7.1840,14.088) (5.9953,16.337)]
        |> filled black
    ] |> group |> scale 0.37 |> move (-2,0)
    

diamond = 
    [ roundedRect 14 20 2 
        |> filled white
        |> addOutline (solid 1.5) red
        |> scale 1.3
        |> move (5,-6)
    , curve (4.7962,0.1498) [Pull (2.3981,-2.847) (0,-5.845),Pull (2.5480,-8.543) (5.0960,-11.24),Pull (7.4941,-8.543) (9.8922,-5.845),Pull (7.3442,-2.847) (4.7962,0.1498)]
      |> filled red
      |> addOutline (solid 0.6) black
    ] |> group |> scale 0.8 |> move (-4,5)
    

snowflake clr clr2 =
    [ curve (-31.77,59.503) [Pull (-34.77,53.658) (-37.77,47.812),Pull (-35.52,41.967) (-33.27,36.121),Pull (-36.57,40.618) (-39.86,45.114),Pull (-43.76,44.215) (-47.66,43.316),Pull (-47.36,39.569) (-47.06,35.822),Pull (-41.06,34.173) (-35.07,32.524),Pull (-37.02,31.475) (-38.96,30.426),Pull (-37.02,26.229) (-35.07,22.032),Pull (-39.86,25.030) (-44.66,28.028),Pull (-44.51,22.782) (-44.36,17.536),Pull (-47.06,20.833) (-49.76,24.131),Pull (-51.85,23.081) (-53.95,22.032),Pull (-52.00,28.028) (-50.06,34.023),Pull (-53.20,36.121) (-56.35,38.220),Pull (-59.20,35.672) (-62.05,33.124),Pull (-59.50,27.578) (-56.95,22.032),Pull (-61.00,26.829) (-65.04,31.625),Pull (-71.64,31.925) (-78.23,32.224),Pull (-74.94,26.978) (-71.64,21.733),Pull (-65.04,20.384) (-58.45,19.035),Pull (-64.44,18.285) (-70.44,17.536),Pull (-71.49,14.088) (-72.54,10.641),Pull (-69.09,9.1428) (-65.64,7.6440),Pull (-61.30,11.990) (-56.95,16.337),Pull (-57.10,14.088) (-57.25,11.840),Pull (-52.75,11.241) (-48.26,10.641),Pull (-52.75,7.6440) (-57.25,4.6463),Pull (-52.90,2.3981) (-48.56,0.1498),Pull (-52.90,-0.449) (-57.25,-1.049),Pull (-57.10,-3.147) (-56.95,-5.245),Pull (-61.15,-1.049) (-65.34,3.1475),Pull (-68.94,1.6487) (-72.54,0.1498),Pull (-71.49,-3.597) (-70.44,-7.344),Pull (-64.59,-7.793) (-58.75,-8.243),Pull (-64.89,-9.292) (-71.04,-10.34),Pull (-74.79,-16.18) (-78.53,-22.03),Pull (-71.94,-21.58) (-65.34,-21.13),Pull (-61.15,-16.18) (-56.95,-11.24),Pull (-59.20,-16.48) (-61.45,-21.73),Pull (-58.90,-24.73) (-56.35,-27.72),Pull (-53.20,-25.33) (-50.06,-22.93),Pull (-51.85,-17.23) (-53.65,-11.54),Pull (-51.70,-12.44) (-49.76,-13.33),Pull (-47.06,-9.892) (-44.36,-6.444),Pull (-44.36,-11.69) (-44.36,-16.93),Pull (-39.71,-14.08) (-35.07,-11.24),Pull (-37.02,-15.73) (-38.96,-20.23),Pull (-36.87,-21.28) (-34.77,-22.33),Pull (-40.76,-23.53) (-46.76,-24.73),Pull (-47.21,-28.92) (-47.66,-33.12),Pull (-44.06,-33.87) (-40.46,-34.62),Pull (-36.87,-29.97) (-33.27,-25.33),Pull (-35.52,-31.17) (-37.77,-37.02),Pull (-34.62,-42.86) (-31.47,-48.71),Pull (-28.47,-43.01) (-25.48,-37.32),Pull (-27.57,-31.32) (-29.67,-25.33),Pull (-26.37,-29.97) (-23.08,-34.62),Pull (-19.18,-33.57) (-15.28,-32.52),Pull (-15.88,-28.62) (-16.48,-24.73),Pull (-22.18,-23.38) (-27.87,-22.03),Pull (-25.92,-20.98) (-23.98,-19.93),Pull (-25.77,-15.73) (-27.57,-11.54),Pull (-23.08,-14.38) (-18.58,-17.23),Pull (-18.58,-11.84) (-18.58,-6.444),Pull (-15.88,-10.04) (-13.18,-13.63),Pull (-11.09,-12.44) (-8.992,-11.24),Pull (-10.94,-17.08) (-12.88,-22.93),Pull (-9.742,-25.33) (-6.594,-27.72),Pull (-3.896,-24.88) (-1.199,-22.03),Pull (-3.597,-16.63) (-5.995,-11.24),Pull (-1.798,-16.33) (2.3981,-21.43),Pull (8.8430,-21.58) (15.288,-21.73),Pull (11.840,-16.18) (8.3934,-10.64),Pull (1.9484,-9.442) (-4.496,-8.243),Pull (1.4988,-7.793) (7.4941,-7.344),Pull (8.5433,-3.447) (9.5925,0.4496),Pull (5.9953,1.7985) (2.3981,3.1475),Pull (-1.798,-1.199) (-5.995,-5.545),Pull (-5.845,-3.297) (-5.695,-1.049),Pull (-10.34,-0.449) (-14.98,0.1498),Pull (-10.19,2.8477) (-5.395,5.5456),Pull (-10.04,8.0936) (-14.68,10.641),Pull (-10.19,11.091) (-5.695,11.540),Pull (-5.845,13.939) (-5.995,16.337),Pull (-1.798,11.840) (2.3981,7.3442),Pull (5.9953,8.9929) (9.5925,10.641),Pull (8.5433,14.238) (7.4941,17.836),Pull (1.4988,18.435) (-4.496,19.035),Pull (1.9484,20.234) (8.3934,21.433),Pull (11.840,26.978) (15.288,32.524),Pull (8.8430,32.224) (2.3981,31.925),Pull (-1.798,26.829) (-5.995,21.733),Pull (-3.597,27.278) (-1.199,32.824),Pull (-3.896,35.522) (-6.594,38.220),Pull (-9.742,35.971) (-12.88,33.723),Pull (-11.09,27.728) (-9.292,21.733),Pull (-11.09,22.932) (-12.88,24.131),Pull (-15.73,20.683) (-18.58,17.236),Pull (-18.58,22.482) (-18.58,27.728),Pull (-23.08,25.180) (-27.57,22.632),Pull (-25.77,26.679) (-23.98,30.725),Pull (-26.22,31.775) (-28.47,32.824),Pull (-22.33,34.323) (-16.18,35.822),Pull (-15.73,39.569) (-15.28,43.316),Pull (-19.03,44.215) (-22.78,45.114),Pull (-26.37,40.318) (-29.97,35.522),Pull (-27.72,41.817) (-25.48,48.112),Pull (-28.77,53.807) (-31.77,59.503)]
        |> outlined (solid 3) clr
    , curve (-31.47,16.936) [Pull (-27.12,18.585) (-22.78,20.234),Pull (-22.03,15.737) (-21.28,11.241),Pull (-17.53,8.2435) (-13.78,5.2459),Pull (-17.53,2.3981) (-21.28,-0.449),Pull (-21.88,-5.245) (-22.48,-10.04),Pull (-26.97,-8.393) (-31.47,-6.744),Pull (-35.82,-8.543) (-40.16,-10.34),Pull (-40.91,-5.545) (-41.66,-0.749),Pull (-45.41,2.2482) (-49.16,5.2459),Pull (-45.41,8.0936) (-41.66,10.941),Pull (-40.91,15.737) (-40.16,20.533),Pull (-35.97,18.735) (-31.47,16.936)]
        |> filled clr
    , curve (-30.27,7.6440) [Pull (-29.97,10.941) (-29.67,14.238),Pull (-27.42,14.988) (-25.18,15.737),Pull (-24.88,13.789) (-24.58,11.840),Pull (-27.42,9.7423) (-30.27,7.6440)]
        |> filled clr2
    , curve (-28.47,5.2459) [Pull (-25.62,6.7447) (-22.78,8.2435),Pull (-20.83,6.7447) (-18.88,5.2459),Pull (-20.68,3.8969) (-22.48,2.5480),Pull (-25.48,3.8969) (-28.47,5.2459)]
        |> filled clr2
    , curve (-29.97,2.8477) [Pull (-27.27,0.8992) (-24.58,-1.049),Pull (-24.88,-3.147) (-25.18,-5.245),Pull (-27.27,-4.496) (-29.37,-3.747),Pull (-29.67,-0.599) (-29.97,2.8477)]
        |> filled clr2
    , curve (-32.97,2.8477) [Pull (-33.27,-0.449) (-33.57,-3.747),Pull (-35.67,-4.496) (-37.77,-5.245),Pull (-38.07,-3.297) (-38.37,-1.348),Pull (-35.67,0.7494) (-32.97,2.8477)]
        |> filled clr2
    , curve (-34.47,5.5456) [Pull (-37.32,6.7447) (-40.16,7.9437),Pull (-41.96,6.5948) (-43.76,5.2459),Pull (-42.11,3.7470) (-40.46,2.2482),Pull (-37.62,3.8969) (-34.47,5.5456)]
        |> filled clr2
    , curve (-37.77,16.037) [Pull (-38.07,13.939) (-38.37,11.840),Pull (-35.52,9.7423) (-32.67,7.6440),Pull (-32.97,10.941) (-33.27,14.238),Pull (-35.52,15.138) (-37.77,16.037)]
        |> filled clr2
    ] |> group


score model = 
    [ roundedRect 50 15 3
        |> filled (hsl (degrees 217) 0.383 0.158)
        |> addOutline (dotted 1) white
        |> move (-35,-30)
    , text "Score : "
        |> bold
        |> filled white
        |> scale 0.6
        |> move (-55,-32)
    , text (String.fromInt model.score)
        |> bold
        |> filled white
        |> scale 0.6
        |> move (-30,-32)
    , roundedRect 40 15 3
        |> filled (hsl (degrees 217) 0.383 0.158)
        |> addOutline (dotted 1) white
        |> move (35,-30)
        , text "Time : "
        |> bold
        |> filled white
        |> scale 0.6
        |> move (20,-32)
    , text (String.fromInt (floor (model.time - model.startTime))) 
        |> bold
        |> filled white
        |> scale 0.6
        |> move (42,-32)
    ] |> group 


snowflakes = [ snowflake (hsl (degrees 217) 0.383 0.158) white
               |> makeTransparent 0.7
               |> scale 1
               |> move (-40,30)
            , snowflake (hsl (degrees 217) 0.383 0.158) white
               |> makeTransparent 0.7
               |> scale 1
               |> move (90,-40)
            , snowflake (hsl (degrees 217) 0.383 0.158) white
               |> makeTransparent 0.7
               |> scale 0.5
               |> move (60,50)
            , snowflake (hsl (degrees 217) 0.383 0.158) white
               |> makeTransparent 0.7
               |> scale 0.5
               |> move (-30,-70) ] |> group

exitButton = 
    [  roundedRect 25 13 3
        |> filled (hsl (degrees 217) 0.383 0.158)
        |> addOutline (dotted 1) white
        |> move (0,0)
    , text "EXIT"
        |> bold
        |> filled white
        |> scale 0.6
        |> move (-9,-2)
    ] |> group |> move (79,54) |> notifyTap Exit

cards1 = [3,1,2,1,4,3,2,4]
cards2 = [1,3,2,1,4,3,2,4]
cards3 = [4,1,3,2,1,4,3,2]
cards4 = [1,4,2,1,3,4,2,3]

