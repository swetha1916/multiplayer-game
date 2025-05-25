module Shop exposing (..)

myShapes model =
  [
    -- Background
    rect 900 900
        |> filled (hsl (degrees 197) 0.284 0.624)
    , leftCurtain
        |> scale 1.5
        |> move (31.5,-10) 
    , rightCurtain
        |> scale 1.3
        |> move (3,0) 
    , titleText model
        |> move (0, 50)
    , button model "Exit" Exit
        |> move (-75,54)
    , displayCoins model
    , loadItems model (getPage pages model.area)
        |> move (-52, 0)
    ,  [
        triangle 6
         |> filled green
         |> addOutline (solid 1) (if model.hover == "Right" then white
                                  else black)
         |> notifyTap (NextArea 1)
         |> notifyEnter (MouseHover "Right")
         |> notifyLeave (MouseExit)
         |> move (10, 0)
     ,  triangle 6
         |> filled green
         |> addOutline (solid 1) (if model.hover == "Left" then white
                                  else black)
         |> notifyTap (NextArea -1)
         |> notifyEnter (MouseHover "Left")
         |> notifyLeave (MouseExit)
         |> rotate (degrees 180)
         |> move (-10, 0)
     ] |> group |> move (0, -52)
  ]

                                             
                    
---- Predefined Values ----

-- Title Values
areas = ["Trophies!", "Whack-a-mole"]
pages = [backgrounds, hammers]

-- Backgrounds

backgrounds = [ defaultBg, redBg, greenBg, blueBg ]
defaultBg = { name = "Default", price = 10, shape = trophy red (hsl (degrees 234) 0.038 0.573) |> scale 0.4 |> move (-1,3) }
redBg = { name = "Golden", price = 300, shape = trophy (hsl (degrees 45) 0.973 0.579) (hsl (degrees 45) 0.903 0.485) |> scale 0.6 |> move (-1,5) }
greenBg = { name = "Silver", price = 225, shape = trophy (hsl (degrees 234) 0.026 0.713) (hsl (degrees 234) 0.038 0.573) |> scale 0.6 |> move (-1,5) }
blueBg = { name = "Bronze", price = 150, shape = trophy (hsl (degrees 30) 0.546 0.474) (hsl (degrees 30) 0.575 0.386) |> scale 0.6 |> move (-1,5) }

-- Whack a Mole
hammers = [hammerD, hammerA, hammerB, hammerC]
hammerD = { name = "Blue Ham", price = 100, shape = [ hammer blue
                                                       ] |> group }
hammerA = { name = "Red Ham", price = 100, shape = [ hammer red
                                                       ] |> group }
hammerB = { name = "Grn Ham", price = 100, shape = [ hammer green
                                                       ] |> group }                                                       
hammerC = { name = "Yelw Ham", price = 100, shape = [ hammer yellow
                                                       ] |> group }

button model t noti = [ roundedRect 30 12 2
                         |> filled orange
                         |> addOutline (solid 0.5) (if model.hover == t then white
                                                    else black)
                   ,  text t 
                         |> centered
                         |> filled black
                         |> scale 0.8
                         |> move (0, -3)
                   ] |> group |> notifyTap noti
                              |> notifyEnter (MouseHover t)
                              |> notifyLeave (MouseExit)

warning t = [ roundedRect 50 12 2
                         |> filled orange
                         |> addOutline (solid 0.5) black
                   ,  text t 
                         |> centered
                         |> filled black
                         |> scale 0.8
                         |> move (0, -3)
                   ] |> group

---- Predefined Functions ----

displayCoins model = [            
                  roundedRect 50 10 2
                  |> filled green 
                  |> addOutline (solid 0.5) black
                  |> move (0, 3)
                , text ("Coins: " ++ (String.fromInt model.coins) )
                  |> sansserif |> centered 
                  |> filled black
                  |> scale 0.8
               ] |> group |> move (70,50)


---- Calculations ----
-- Switch to the next area based on index change
-- Find the initial and last pages
switchArea model x =
    let maxIndex = (List.length areas) - 1
        nextIndex = model.area + x
    in
    if (x /= 1)  && (x /= -1) then model.area
    else 
      -- At the last page, go to first page
      -- At the first page, go to last page
      (if nextIndex > maxIndex then 0
       else if nextIndex < 0 then maxIndex
       else nextIndex)

-- Get element based on list index
getArea ys i =
  case ys of
    (x :: xs) -> if i == 0 then x
                 else getArea xs (i - 1)
    _ -> ""
    
getPage list i =
  case list of
    (x :: xs) -> if i == 0 then x
                 else getPage xs (i - 1)
    _ -> []

---- Rendering Functions ----

titleText model = [
                    text "Shop"
                      |> centered
                      |> filled black
                      |> addOutline (solid 0.5) black
                  , text (getArea areas model.area)
                      |> centered
                      |> filled black
                      |> move (0, -12)
                  ] |> group

-- Display each item (object) with text below it recursively
loadItems model items = case items of
      (x::xs) -> [
                        let { name, price, shape } = x
                        in
                        [
                          shape
                            |> addOutline (solid 1) (if model.hover == name then white
                                                     else black)
                        , text name
                            |> size 8
                            |> centered
                            |> filled black
                            |> move (0,-16)
                        , text (if not (List.member name model.inventory)
                               then (String.fromInt (price) ++ "C" )
                               else "Owned!" )
                            |> size 8
                            |> centered
                            |> filled (if not (List.member name model.inventory)
                                       then red
                                       else darkGreen )
                            |> move (0,-23)
                        , (if (List.member name model.inventory) || model.hover /= name
                           then [] |> group
                           else if not (List.member name model.inventory) && model.coins - price < 0
                           then warning "Not enough!" |> move (0, 20)
                           else warning "Buy?" |> move (0, 20) )
                        ] |> group
                          |> notifyEnter (MouseHover name)
                          |> notifyLeave (MouseExit)
                          |> notifyTap (Buy name price)
                        , (loadItems model xs)
                            |> move (35, 0)
                        ] |> group
      _ -> [] |> group

---------------------------------------------

type alias Item = { name : String
                  , price : Int
                  , shape : Stencil
                  }

---- Game Model ----

type Msg = Tick Float GetKeyState
         | MouseHover String
         | MouseExit
         | NextArea Int
         | Buy String Int
         | ChooseHammer Color
         | Exit

type alias Model = { time : Float 
                   , hover : String -- Which button is the user hovering over
                   , area : Int -- Which area is the user in by index
                   , exit : Bool
                   , coins : Int
                   , inventory : List (String)
                   , hammerC : Color
                   }

update msg model = case msg of
                     Tick t _ -> { model | time = t }
                     MouseHover s -> { model | hover = s }
                     MouseExit -> { model | hover = "" }
                     NextArea x -> { model | area = (switchArea model x) }
                     Exit -> { model | exit = True }
                     ChooseHammer c -> { model | hammerC = (if c == red && (List.member "Red Ham" model.inventory)
                                                            then red
                                                            else if c == green && (List.member "Grn Ham" model.inventory)
                                                            then green 
                                                            else if c == yellow && (List.member "Yelw Ham" model.inventory)
                                                            then yellow 
                                                            else if c == blue
                                                            then blue
                                                            else model.hammerC) }
                     Buy x cost -> (if (model.coins - cost < 0) then
                                   model
                                   else 
                                   { model | inventory = model.inventory ++ [x]
                                             , coins = model.coins - cost }
                                   )

init = { time = 0 
       , hover = ""
       , area = 0
       , exit = False
       , coins = 100
       , inventory = [ "Default", "Blue Ham" ]
       , hammerC = blue
       }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)


-------------------------------------

leftCurtain =
    [ curve (-58.25,54.881) [Pull (-58.47,46.606) (-61.29,38.332),Pull (-63.92,30.733) (-69.06,23.134),Pull (-71.25,19.199) (-77.84,15.704),Pull (-78.79,14.184) (-78.86,12.664),Pull (-78.92,10.976) (-78.86,9.2875),Pull (-76.74,4.5593) (-75.14,-0.168),Pull (-74.19,-3.030) (-73.11,-5.572),Pull (-71.72,-8.443) (-71.09,-11.31),Pull (-69.61,-15.70) (-69.06,-20.09),Pull (-68.73,-23.64) (-67.71,-27.18),Pull (-67.08,-28.20) (-67.37,-29.21),Pull (-68.72,-29.32) (-70.07,-28.87),Pull (-71.09,-27.91) (-72.10,-28.20),Pull (-72.85,-28.39) (-73.11,-28.87),Pull (-73.96,-29.99) (-74.80,-30.22),Pull (-75.65,-30.46) (-76.49,-29.55),Pull (-77.34,-28.66) (-78.18,-28.53),Pull (-79.19,-28.37) (-80.21,-29.21),Pull (-81.05,-29.98) (-81.89,-30.22),Pull (-82.91,-30.33) (-83.92,-29.88),Pull (-84.33,-29.38) (-84.26,-28.87),Pull (-84.26,13.171) (-84.26,55.218),Pull (-71.26,55.050) (-58.25,54.881),Pull (-56.56,54.374) (-58.25,54.881)]
          |> filled red
          |> addOutline (solid 1) (hsl (degrees 0) 0.755 0.361)
    , curve (-22.80,49.533) [Pull (-23.74,43.243) (-24.68,36.953),Pull (-25.63,32.078) (-26.57,27.203),Pull (-27.16,23.115) (-28.14,19.027),Pull (-29.51,12.265) (-31.60,5.5036),Pull (-32.55,2.0442) (-33.49,-1.415),Pull (-29.12,18.712) (-28.14,38.840),Pull (-28.30,44.501) (-28.46,50.162),Pull (-25.78,50.004) (-22.80,49.533)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> scale 0.7
          |> move (-60,20)
    , curve (-14.62,49.847) [Pull (-14.45,41.356) (-15.25,32.864),Pull (-15.89,27.046) (-17.76,21.228),Pull (-19.57,15.410) (-21.85,9.5921),Pull (-25.31,4.0884) (-28.77,-1.415),Pull (-21.14,11.950) (-19.02,25.316),Pull (-18.07,31.606) (-18.39,37.896),Pull (-19.02,44.186) (-19.65,50.476),Pull (-17.14,50.162) (-14.62,49.847)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> scale 0.7
          |> move (-55,20)
    , curve (-29.40,49.533) [Pull (-28.78,42.299) (-29.40,35.066),Pull (-29.29,29.562) (-30.66,24.058),Pull (-32.39,18.083) (-34.12,12.108),Pull (-29.62,20.174) (-27.20,29.719),Pull (-25.60,35.695) (-25.31,41.670),Pull (-25.31,46.073) (-25.31,50.476),Pull (-27.36,50.319) (-29.40,49.533)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> move (-32,20)
    , curve (-32.55,49.533) [Pull (-31.78,40.255) (-33.49,30.977),Pull (-34.13,26.732) (-35.38,22.486),Pull (-36.63,19.184) (-37.89,15.882),Pull (-34.90,22.800) (-31.92,29.719),Pull (-30.10,36.009) (-29.40,42.299),Pull (-29.40,45.916) (-29.40,49.533),Pull (-31.44,49.690) (-32.55,49.533)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> scale 0.7
          |> move (-50,20)
    , curve (-37.89,-63.37) [Pull (-37.05,-55.19) (-37.58,-47.01),Pull (-37.58,-41.82) (-37.58,-36.63),Pull (-37.73,-34.28) (-37.89,-31.92),Pull (-36.70,-38.52) (-36.63,-45.13),Pull (-35.76,-52.20) (-35.69,-59.28),Pull (-35.69,-61.01) (-35.69,-62.74),Pull (-36.95,-62.74) (-37.89,-63.37)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> scale 1
          |> move (-42,34.6)
    , curve (-10.85,-54.87) [Pull (-9.790,-48.43) (-10.85,-41.98),Pull (-11.95,-36.16) (-13.05,-30.34),Pull (-9.781,-39.62) (-9.592,-48.90),Pull (-9.277,-51.73) (-8.963,-54.56),Pull (-9.906,-54.56) (-10.85,-54.87)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> scale 1
          |> move (-62,27)
    , rectangle 15 10
          |> filled yellow
          |> scale 0.5
          |> move (-81.5,13)
    ] |> group 

rightCurtain = 
    [ curve (18.555,63.056) [Pull (21.541,46.073) (29.248,29.090),Pull (31.750,22.800) (35.852,16.511),Pull (37.582,14.152) (39.312,11.793),Pull (40.210,11.164) (41.828,10.535),Pull (42.299,9.2776) (42.771,8.0196),Pull (42.771,7.3906) (42.771,6.7616),Pull (41.828,5.2774) (40.884,2.6732),Pull (38.909,-2.515) (38.054,-7.705),Pull (37.425,-13.52) (36.796,-19.34),Pull (36.324,-23.90) (35.852,-28.46),Pull (35.380,-30.92) (34.909,-32.86),Pull (34.280,-34.12) (33.651,-35.38),Pull (33.442,-36.63) (34.594,-37.89),Pull (35.223,-38.21) (35.852,-38.52),Pull (37.101,-37.58) (37.110,-36.63),Pull (37.393,-38.05) (36.796,-39.46),Pull (37.425,-40.40) (38.054,-40.09),Pull (39.469,-39.95) (40.884,-40.41),Pull (41.670,-40.72) (42.457,-41.04),Pull (43.243,-41.18) (44.029,-40.41),Pull (44.501,-39.62) (44.972,-38.84),Pull (45.916,-38.33) (46.859,-39.15),Pull (46.999,-40.41) (46.859,-41.67),Pull (47.488,-42.56) (48.117,-42.29),Pull (49.533,-41.98) (50.948,-41.98),Pull (50.948,-14.30) (50.948,13.366),Pull (51.420,38.368) (51.891,63.371),Pull (35.066,63.056) (18.555,63.056)]
          |> filled red
          |> addOutline (solid 1) (hsl (degrees 0) 0.755 0.361)
          |> move (20,-2)
    , curve (41.199,60.226) [Pull (41.779,51.891) (43.400,43.557),Pull (45.139,35.538) (48.117,27.518),Pull (49.375,23.429) (50.633,19.341),Pull (47.186,33.965) (44.658,48.589),Pull (44.238,54.407) (44.658,60.226),Pull (42.928,60.697) (41.199,60.226)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> move (10,0) 
    , curve (27.046,55.508) [Pull (28.445,47.331) (31.764,39.154),Pull (32.664,37.739) (33.965,36.324),Pull (31.298,42.614) (30.191,48.904),Pull (29.877,52.049) (29.562,55.194),Pull (28.304,55.194) (27.046,55.508)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> move (18,-6) 
    , curve (39.941,54.565) [Pull (40.859,44.658) (42.457,34.751),Pull (42.728,30.663) (43.400,26.574),Pull (44.495,22.171) (46.230,17.769),Pull (42.986,32.393) (42.142,47.017),Pull (42.299,50.948) (42.457,54.879),Pull (41.199,54.879) (39.941,54.565)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> rotate (degrees -3)
          |> scale 1.5
          |> move (0,0)   
    , curve (-25.15,-47.01) [Pull (-21.81,-39.62) (-21.07,-32.23),Pull (-20.75,-25.94) (-20.44,-19.65),Pull (-19.09,-32.55) (-20.75,-45.44),Pull (-22.95,-45.91) (-25.15,-47.01)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> move (80.7,6)
    , curve (-38.99,-46.07) [Pull (-36.03,-40.09) (-34.59,-34.12),Pull (-35.38,-39.62) (-36.16,-45.13),Pull (-37.73,-45.13) (-38.99,-46.07)]
          |> filled (hsl (degrees 0) 0.755 0.361)
          |> move (103,5)
    , rectangle 17 9
          |> filled yellow
          |> scale 0.6
          |> move (66.5,6)
    ] |> group

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
            |> addOutline (solid 2) white
            |> scale 0.5
            |> move (-32,0)
      ] |> group |> scale 0.8 |> notifyTap (ChooseHammer col)
    
    
trophy clr1 clr2 =
    [ curve (18.885,32.524) [Pull (19.407,21.212) (14.088,12.740),Pull (11.701,9.0023) (8.3934,6.7447),Pull (8.2837,5.5456) (7.4941,4.3466),Pull (7.0647,3.5971) (5.9953,2.8477),Pull (6.0148,0) (7.1943,-2.847),Pull (9.1327,-6.796) (11.391,-7.344),Pull (11.840,-8.093) (12.290,-8.843),Pull (3.8969,-8.843) (-4.496,-8.843),Pull (-4.796,-8.843) (-5.096,-8.843),Pull (-4.946,-8.093) (-4.796,-7.344),Pull (-1.388,-5.096) (-0.299,-2.847),Pull (1.2696,0) (1.1990,2.8477),Pull (0.4496,3.1771) (-0.299,4.3466),Pull (-0.999,5.5456) (-0.899,6.7447),Pull (-3.147,7.4134) (-5.395,10.042),Pull (-8.504,13.489) (-9.292,16.936),Pull (-11.69,24.880) (-11.09,32.824),Pull (3.7470,32.674) (18.885,32.524)]
        |> filled clr1
        |> addOutline (solid 2) clr2
    , curve (-6.894,-8.843) [Pull (-7.044,-13.93) (-7.194,-19.03),Pull (-7.194,-19.33) (-7.194,-19.63),Pull (3.5971,-19.63) (14.388,-19.63),Pull (14.388,-14.38) (14.388,-9.142),Pull (3.7470,-9.142) (-6.894,-8.843)]
        |> filled (hsl (degrees 45) 0.044 0.363)
        |> addOutline (solid 2) black
    , curve (-8.992,-19.63) [Pull (3.5971,-19.63) (16.187,-19.63),Pull (16.187,-20.68) (16.187,-21.73),Pull (3.5971,-21.88) (-8.992,-22.03),Pull (-8.992,-20.83) (-8.992,-19.63)]
        |> filled (hsl (degrees 45) 0.044 0.363)
        |> addOutline (solid 1) black
    , curve (18.585,30.725) [Pull (19.134,33.724) (21.882,34.922),Pull (24.251,35.442) (25.779,34.323),Pull (28.608,32.224) (28.477,30.126),Pull (28.848,27.728) (27.578,25.330),Pull (26.190,22.932) (23.081,20.533),Pull (20.983,19.125) (18.885,17.236),Pull (17.915,16.037) (18.585,14.838),Pull (19.634,14.198) (20.683,14.838),Pull (21.453,14.238) (20.983,13.639),Pull (19.934,12.709) (18.885,12.740),Pull (17.686,12.549) (16.487,14.238),Pull (16.117,15.288) (16.187,16.337),Pull (17.536,19.025) (18.885,19.634),Pull (21.473,21.733) (23.981,23.831),Pull (26.620,26.079) (26.978,28.327),Pull (27.289,30.276) (26.079,32.224),Pull (24.880,33.724) (23.081,33.423),Pull (21.882,33.494) (20.683,31.925),Pull (20.353,31.175) (20.384,30.426),Pull (19.934,29.686) (19.484,29.826),Pull (18.885,29.716) (18.585,30.725)]
        |> filled clr1
        |> addOutline (solid 1) clr2
    , curve (-11.39,30.725) [Pull (-11.54,33.864) (-14.08,34.922),Pull (-16.33,35.682) (-18.58,34.323),Pull (-20.86,33.234) (-21.58,30.426),Pull (-21.59,28.327) (-20.68,26.229),Pull (-19.39,23.831) (-16.78,21.433),Pull (-14.01,19.334) (-11.69,17.236),Pull (-10.84,16.187) (-10.79,15.138),Pull (-10.85,14.988) (-10.79,14.838),Pull (-11.69,14.238) (-12.59,14.838),Pull (-13.50,14.238) (-13.18,13.639),Pull (-11.84,12.649) (-10.49,12.740),Pull (-8.892,13.659) (-8.693,15.138),Pull (-8.612,16.187) (-9.292,17.236),Pull (-10.72,19.185) (-13.18,21.133),Pull (-18.58,24.880) (-19.78,28.627),Pull (-19.85,30.546) (-18.28,32.224),Pull (-16.78,33.404) (-15.28,33.423),Pull (-13.73,33.254) (-13.18,31.925),Pull (-13.02,31.175) (-12.59,30.426),Pull (-12.29,29.936) (-11.99,30.126),Pull (-11.43,30.246) (-11.39,30.725)]
        |> filled clr1
        |> addOutline (solid 1) clr2
    ] |> group