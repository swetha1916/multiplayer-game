module WhackAMole exposing (..)

import Random exposing (..)

myShapes model =
  [ -- bg
    rect 900 900
        |> filled (hsl (degrees 186) 0.534 0.72) 
  , rect 900 900 -- Detect mouse movement for hammer
        |> ghost
        |> notifyMouseMoveAt (MouseMove)
  , button model "Exit" Exit
        |> move (-80,52) |> notifyMouseMoveAt (MouseMove)
  ]  
  ++
  
  (case model.gamestate of
      Start ->  
          [ 
            row3holes model 9
                |> group
          , row2holes model 6
                |> group 
          , row1holes model 3
                |> group
          , scoretimeText model
                |> scale 0.5
                |> move (65,50) |> notifyMouseMoveAt (MouseMove)
          , whackText 
                |> scale 1
                |> move (0,50) |> notifyMouseMoveAt (MouseMove)
          , hammer (model.col)
                |> move (model.pt)
                |> addOutline (solid 2) black 
          ]                      
      End -> 
          [ text "GAME OVER!!"
              |> centered
              |> bold
              |> italic
              |> filled blue
              |> addOutline (solid 0.2) black
              |> scale 2 
          , text "Your score: "
              |> centered
              |> bold
              |> italic
              |> filled red
              |> addOutline (solid 0.2) black
              |> move (-20,-20)
          , text (String.fromInt model.score)
              |> centered
              |> bold
              |> italic
              |> filled red
              |> addOutline (solid 0.2) black
              |> move (20,-20)
          , text "Coins earned: "
              |> centered
              |> bold
              |> italic
              |> filled red
              |> addOutline (solid 0.2) black
              |> move (-20,-40)
          , text (String.fromInt model.coins)
              |> centered
              |> bold
              |> italic
              |> filled red
              |> addOutline (solid 0.2) black
              |> move (20,-40)
          ]
  )          

----------------------------------------------------------------------------

button model t noti = [ roundedRect 30 12 2
                         |> filled white
                         |> addOutline (solid 0.5) black
                   ,  text t 
                         |> centered
                         |> filled black
                         |> scale 0.8
                         |> move (0, -3)
                   ] |> group |> notifyTap noti

getElement els i =
  case els of
    (x :: xs) -> if i == 1 then x
                 else getElement xs (i - 1)
    [] -> None

replaceElement els i value =
  case els of
    (x :: xs) -> if i == 1 then [value] ++ xs
                 else [x] ++ (replaceElement xs (i - 1) value)
    [] -> []

------------------------------------------------------------------

getSeed model = initialSeed (round <| model.time * 1000)

moleGenerator : Generator (List MoleState)
moleGenerator = Random.list 9 (Random.uniform MoleDown [MoleUp])

randomMole model t = 
  let (xs, newSeed) = Random.step moleGenerator model.seed
  in
  { model | time = t, moles = xs, seed = newSeed, intervalTime = t }

--------------------------------------------------------------------------------

type GameState = Start
               | End

type Msg = Tick Float GetKeyState
         | HitMole Int MoleState
         | MouseOn Int
         | MouseOff
         | MouseMove (Float,Float)
         | Exit


type MoleState = MoleUp
               | MoleDown
               | None


type alias Model = { time : Float
                   , score : Int
                   , coins : Int
                   , gamestate : GameState
                   , mouse : Maybe Int 
                   , pt : (Float,Float)
                   , moles : List (MoleState)
                   , seed : Seed
                   , intervalTime : Float
                   , startTime : Float
                   , exit : Bool 
                   , color : Color}


update msg model = case msg of
                     Tick t _ -> if t - model.startTime > 30 then
                                     { model | time = t, gamestate = End, coins = round ((toFloat model.score) / 4) }
                                 else if (round model.intervalTime) == (round t) - 2 then
                                     randomMole model t
                                 else
                                     { model | time = t }
                     MouseOn m -> { model | mouse = Just m }
                     MouseOff -> { model | mouse = Nothing }
                     HitMole n state -> case state of
                                        MoleDown ->
                                          { model | moles = (replaceElement model.moles n state)
                                                  , score = model.score + 2 }
                                        MoleUp -> { model | score = model.score - 1 }
                                        None -> model
                     MouseMove x -> { model | pt = x }
                     Exit -> { model | exit = True }
      
init = { time = 0
       , score = 0 
       , gamestate = Start
       , pt = (0,0)
       , mouse = Nothing
       , moles = (List.repeat 9 MoleDown)
       , seed = initialSeed 0
       , startTime = 0
       , intervalTime = 0
       , coins = 0
       , exit = False
       , col = blue }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

-------------------------------------------------------------------------------
whackText = 
    [ text "Whack-a-Mole"
          |> centered
          |> filled black
          |> addOutline (solid 0.5) black
    ] |> group

scoretimeText model =
    [ roundedRect 38 22 3
          |> filled grey
          |> addOutline (solid 1) black
          |> scale 2
          |> move (8,-5)
    , text "SCORE: "
          |> centered
          |> filled black
          |> addOutline (solid 0.5) black
    , text (String.fromInt model.score)
          |> centered
          |> filled red
          |> addOutline (solid 0.5) red
          |> move (30,0)
    , text "TIME LEFT: "
          |> centered
          |> filled black
          |> addOutline (solid 0.5) black
          |> scale 0.8
          |> move (1,-15)
    , text (String.fromInt (30 - (floor (model.time - model.startTime))))
          |> centered
          |> filled red
          |> addOutline (solid 0.5) red
          |> move (36,-15)
    ] |> group
    
    
row1holes model n = 
    if n == 0 then
        []
    else
        [ circle 10
            |> filled (hsl (degrees 233) 0.082 0.279)
            |> addOutline (solid 2) orange
            |> scaleX 3.25
            |> scale 0.8
            |> scaleY 1.5
            |> move (-68,-40) |> notifyMouseMoveAt (MouseMove)
        , (if (getElement model.moles n) == MoleDown then hidemole (model.mouse == Just n) n 
           else mole (model.mouse == Just n) n )
            |> scale 0.7
            |> move (-80,-10)
        , row1holes model (n - 1)
            |> group 
            |> move (68,0) 
        ]
       
       
row2holes model n = 
    if n == 3 then
        []
    else
        [ circle 10
            |> filled (hsl (degrees 233) 0.07 0.238)
            |> addOutline (solid 2) red
            |> scaleX 3.25
            |> scale 0.65
            |> scaleY 1.5
            |> move (-55,-12.5) |> notifyMouseMoveAt (MouseMove)
        , (if (getElement model.moles n) == MoleDown then hidemole (model.mouse == Just n) n 
           else mole (model.mouse == Just n) n )
            |> scale 0.6
            |> move (-65,15)
        , row2holes model (n - 1)
            |> group 
            |> move (55,0) ]
   
   
row3holes model n = 
    if n == 6 then
        []
    else
        [ circle 10
            |> filled (hsl (degrees 233) 0.094 0.202)
            |> addOutline (solid 2) purple
            |> scaleX 3.25
            |> scale 0.5
            |> scaleY 1.5
            |> move (-45,9) |> notifyMouseMoveAt (MouseMove)
        , (if (getElement model.moles n) == MoleDown then hidemole (model.mouse == Just n) n 
           else mole (model.mouse == Just n) n )
            |> scale 0.5
            |> move (-54,30)
        , row3holes model (n - 1)
            |> group 
            |> move (45,0) ]  


hammer col = 
    [ roundedRect 5 29 2
          |> filled (hsl (degrees 214) 0.129 0.608)
          |> addOutline (solid 1) black
    , rect 6 4
          |> filled (hsl (degrees 233) 0.07 0.238)
          |> addOutline (solid 0.5) black
          |> scale 1.5
          |> move (0,10)
    , roundedRect 17 11 4
          |> filled col
          |> addOutline (solid 0.8) black
          |> scale 1.5
          |> move (0,18)
    , oval 7 20
          |> filled (hsl (degrees 214) 0.787 0.343)
          |> addOutline (solid 1.5) black
          |> scale 0.7
          |> move (10.2,18)
    , curve (49.815,37.319) [Pull (48.182,41.034) (47.788,44.749),Pull (47.755,45.593) (48.802,46.437),Pull (50.659,46.500) (52.517,45.762),Pull (54.712,44.749) (56.907,43.736),Pull (58.089,44.411) (59.271,45.087),Pull (61.298,46.500) (63.324,47.113),Pull (64.675,47.473) (66.026,47.113),Pull (66.832,45.931) (67.039,44.749),Pull (66.026,42.047) (65.013,39.345),Pull (66.870,38.163) (68.728,36.981),Pull (70.856,35.461) (72.105,33.941),Pull (71.261,33.075) (70.416,32.928),Pull (66.364,32.022) (62.311,31.915),Pull (62.091,29.720) (62.311,27.525),Pull (62.311,25.836) (62.311,24.147),Pull (61.635,23.032) (60.960,22.796),Pull (59.440,22.663) (57.920,23.810),Pull (56.232,26.343) (54.543,28.875),Pull (50.997,28.538) (47.451,28.200),Pull (45.775,29.382) (46.100,30.564),Pull (47.397,34.110) (49.815,37.319)]
          |> filled white
          |> addOutline (solid 2) red
          |> scale 0.5
          |> move (-32,0)
    ] |> group |> move (20, -20) |> rotate (degrees 90)


mole outline n = 
    [
      curve (-0.899,-44.51) [Pull (-2.669,-33.12) (-1.199,-21.73),Pull (-0.670,-16.03) (3.2974,-10.34),Pull (8.1129,-5.453) (14.688,-5.245),Pull (17.545,-4.775) (23.081,-5.545),Pull (27.449,-7.173) (29.377,-10.04),Pull (31.605,-11.86) (32.674,-16.33),Pull (34.453,-20.98) (34.473,-25.62),Pull (35.513,-34.77) (34.473,-43.91),Pull (34.473,-44.81) (34.473,-45.71),Pull (26.079,-48.26) (17.686,-47.81),Pull (8.2435,-48.77) (-0.899,-44.51)]
          |> filled brown
          |> addOutline (solid 2) (if outline then white else black)
    , curve (5.0960,-46.31) [Pull (4.9859,-41.51) (5.3957,-36.72),Pull (5.1943,-33.57) (8.9929,-30.42),Pull (11.990,-28.87) (14.988,-28.32),Pull (17.836,-27.71) (20.683,-28.62),Pull (23.951,-29.23) (25.779,-31.92),Pull (27.259,-33.87) (27.578,-35.82),Pull (28.358,-39.11) (28.177,-42.41),Pull (28.177,-44.51) (28.177,-46.61),Pull (22.292,-47.74) (16.487,-47.51),Pull (10.791,-47.89) (5.0960,-46.31)]
          |> filled (hsl (degrees 12) 0.377 0.924)
          |> addOutline (solid 2) black
    , curve (0,-14.83) [Pull (-1.459,-12.59) (-1.199,-10.34),Pull (-1.319,-8.843) (0,-7.344),Pull (1.6487,-5.794) (3.2974,-5.845),Pull (4.6463,-5.775) (5.9953,-6.145),Pull (7.0849,-6.744) (6.8946,-7.344),Pull (5.2459,-8.512) (3.5971,-10.64),Pull (1.7183,-12.32) (0,-14.83)]
          |> filled brown
          |> addOutline (solid 2) (if outline then white else black)
          |> move (0,-0.3)
    , curve (25.779,-6.744) [Pull (27.428,-5.525) (29.077,-5.545),Pull (31.325,-5.474) (33.573,-7.644),Pull (35.223,-9.742) (34.473,-11.84),Pull (34.483,-13.33) (32.374,-14.83),Pull (31.275,-12.42) (29.377,-10.94),Pull (27.128,-8.662) (24.880,-7.344),Pull (25.330,-7.044) (25.779,-6.744)]
          |> filled brown
          |> addOutline (solid 2) (if outline then white else black)
          |> move (0,1)
    , circle 10
          |> filled black
          |> scale 0.2
          |> move (22,-15)
    , circle 10
          |> filled black
          |> scale 0.2
          |> move (10,-15)
    , oval 31 19
          |> filled black
          |> scale 0.23
          |> move (16,-20)
    , oval 22 19
          |> outlined (solid 3) black
          |> scale 0.25
          |> move (13,-22.7)
    , oval 22 19
          |> outlined (solid 3) black
          |> scale 0.25
          |> move (18.7,-22.7)          
    ] |> group |> notifyEnter (MouseOn n) |> notifyLeave (MouseOff) |> notifyTap (HitMole n MoleDown)
      |> notifyMouseMoveAt (MouseMove)

hidemole outline n = 
    [ curve (-37.77,-11.54) [Pull (-23.01,-15.29) (-9.292,-11.24),Pull (-14.23,-1.945) (-23.98,-1.049),Pull (-32.68,-2.825) (-37.77,-11.54)]
          |> filled brown
          |> addOutline (solid 1.3) (if outline then white else black)
          |> move (37,-2.5)
    , curve (0,-14.83) [Pull (-1.459,-12.59) (-1.199,-10.34),Pull (-1.319,-8.843) (0,-7.344),Pull (1.6487,-5.794) (3.2974,-5.845),Pull (4.6463,-5.775) (5.9953,-6.145),Pull (7.0849,-6.744) (6.8946,-7.344),Pull (5.2459,-8.512) (3.5971,-10.64),Pull (1.7183,-12.32) (0,-14.83)]
          |> filled brown
          |> addOutline (solid 1.5) (if outline then white else black)
          |> scale 0.8
          |> move (0,-0.3)
    , curve (25.779,-6.744) [Pull (27.428,-5.525) (29.077,-5.545),Pull (31.325,-5.474) (33.573,-7.644),Pull (35.223,-9.742) (34.473,-11.84),Pull (34.483,-13.33) (32.374,-14.83),Pull (31.275,-12.42) (29.377,-10.94),Pull (27.128,-8.662) (24.880,-7.344),Pull (25.330,-7.044) (25.779,-6.744)]
          |> filled brown
          |> addOutline (solid 1.5) (if outline then white else black)
          |> scale 0.8
          |> move (0,1)
    ] |> group |> move (5,-30)|> notifyEnter (MouseOn n) |> notifyLeave (MouseOff) |> notifyTap (HitMole n MoleUp)
      |> notifyMouseMoveAt (MouseMove)
    
    
    