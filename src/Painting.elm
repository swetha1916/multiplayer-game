module Painting exposing (..)

import Dict exposing (Dict)

myShapes model =
  [
     square 200    
       |> filled (hsl (degrees 273) 0.139 0.731)
  ,  rectangle 200 120
       |> ghost
       |> notifyMouseUp StopDrawing
  , hexagon
  ,  [ paintBrush 
          |> move (55,56)
      , palette
          |> move (50,57)
          |> scale 0.9
      , paintBrush 
          |> move (-62,56)
      , palette
          |> move (-45,57) 
          |> scale 0.9 ] |> group
  
  , paintingText 
  
  ,  button model "Outline" Toggle
       |> move (80,45)
  ,  button model "Exit" Exit
       |> move (-80,45)
  ,  button model "Clear" Clear
       |> move (80,-55)
  ,  button model "Finish!" FinishPainting
       |> move (-80,-55)

  ,  [
        triangle 6
         |> filled purple
         |> addOutline (solid 1) (if model.buttonHover == "Right" then white
                                  else (hsl (degrees 279) 0.145 0.264))
         |> notifyTap (NextOutline 1)
         |> notifyEnter (MouseButtonHover "Right")
         |> notifyLeave (MouseExit)
         |> move (7, -2)
     ,  triangle 6
         |> filled purple
         |> addOutline (solid 1) (if model.buttonHover == "Left" then white
                                  else (hsl (degrees 279) 0.145 0.264))
         |> notifyTap (NextOutline -1)
         |> notifyEnter (MouseButtonHover "Left")
         |> notifyLeave (MouseExit)
         |> rotate (degrees 180)
         |> move (-7, -2)
     ] |> group |> move (0, -52)
  ,  renderCanvas model
  {-,  text (getCoords model.tiles) -- Copy and paste text to save outline
       |> selectable
       |> filled black
       |> move (0, -60) -}
  ,  (if model.outline then renderOutline model (getOutline model.outlines model.outlineIndex)
                               |> move (0, -5)
      else group <| []
      ) 
  ,  (if (model.gameState == End) then 
        renderVictoryBox |> move (0, -52)
      else group <| []
     )
  ,  (if (model.buttonHover == "Finish!") && not (checkIfFinished model) then
        renderWarningBox |> move (0, -52)
      else group <| []
     )
     
 ,  renderColSelector model
       |> move (0, 2)
  ]

-- Get coordinates of drawn image
getCoords dict = 
    let keys = (Dict.keys dict)
    in String.join ","
      <| List.map (\(x, y) -> "(" ++ (String.fromInt x) ++ ", " ++ (String.fromInt y) ++ ")") keys

------- Rendering Functions ---------

renderVictoryBox =
  [
    roundedRect 90 12 2
      |> filled green
      |> addOutline (solid 1) black
  , text "You win!!"
      |> bold
      |> italic
      |> size 6
      |> centered
      |> filled white
      |> move (-25, -2)
  , text ("Reward: 10 Coins")
      |> bold
      |> italic
      |> size 6
      |> centered
      |> filled white
      |> move (15, -2)
  ] |> group |> move (0,-1)

renderWarningBox =
  [
    roundedRect 120 12 2
      |> filled red
      |> addOutline (solid 1) black
      |> move (0,-1.5)
  , text "Please fill in the entire outline!"
      |> bold
      |> italic
      |> size 6
      |> centered
      |> filled white
      |> move (0, -3)
  ] |> group

getOutline outlines i =
  case outlines of
    (x :: xs) -> if i == 0 then x
                 else getOutline xs (i - 1)
    _ -> []
     

renderColours model colours =
  group <|
    List.indexedMap 
      (\n c -> group [ paintBucket c model
                        -- |> addOutline (solid 5) (if model.colour == c then black else if model.hover == c then gray else black)
                     ] 
          |> move (0, -16 * (toFloat n - 0.5 * toFloat (List.length colours)))
          |> notifyTap (PickColour c)
          |> notifyEnter (MouseHover c)
          |> notifyLeave (MouseExit)
      ) colours

renderTiles model =
  let
    xList = List.range (-width // 2) (width // 2)
    yList = List.range (-height // 2) (height // 2)
    getColor (x,y) = case (Dict.get (x,y) model.tiles) of
                            Just c -> c
                            _ -> white
  
  in
    group <| List.concat <|
               List.map (\y -> 
                 List.map (\x -> square (screenWidth / width)
                                   |> filled (getColor (x,y))
                                   |> addOutline (solid 0.5) (getColor(x,y))
                                   |> move (toFloat x * screenWidth / width, toFloat y * screenHeight / height)
                                   |> notifyTap (PaintTile (x,y) model.colour)
                                   |> notifyMouseDown (StartDrawing)
                                   |> notifyMouseUp (StopDrawing)
                                   -- |> notifyTouchStart (StartDrawing)
                                   -- |> notifyTouchEnd (StopDrawing)
                                   |> (
                                       if model.drawing then 
                                         notifyEnter (PaintTile (x,y) model.colour)
                                       else identity
                                      )
                          ) xList
                        ) yList                        

renderOutline model outline = 
  let
    xList = List.range (-width // 2) (width // 2)
    yList = List.range (-height // 2) (height // 2)
  
  in
    group <| List.concat <|
               List.map (\y -> 
                 List.map (\x -> (
                                 if (List.member (x,y) outline) then
                                 square (screenWidth / width)
                                   |> outlined (solid 0.5) black
                                   |> move (toFloat x * screenWidth / width, toFloat y * screenHeight / height)
                                   |> notifyTap (PaintTile (x,y) model.colour)
                                   |> notifyMouseDown (StartDrawing)
                                   |> notifyMouseUp (StopDrawing)
                                 else group []
                                 )
                          ) xList
                        ) yList  

paintTile (x,y) color tiles = 
      Dict.update (x,y) (\_ -> Just color) tiles

renderCanvas model = [ rect (screenWidth) (screenHeight)
                           |> filled green
                           |> addOutline (solid 8) black
                           |> move (0, -5)
                     , renderTiles model
                           |> move (0, -5)
                     ] |> group

renderColSelector model = [ roundedRect 20 80 2
                                    |> filled (hsl (degrees 5) 0.26 0.418)
                                    |> addOutline (solid 1) black
                                    |> move (82,-7)
                           , roundedRect 20 80 2
                                    |> filled (hsl (degrees 5) 0.26 0.418)
                                    |> addOutline (solid 1) black
                                    |> move (-82,-7)
                           , renderColours model coloursL
                                    |> move (-80,-17)
                           , renderColours model coloursR
                                    |> move (83,-17)
                           ] |> group

--------- Predefined Values ---------

blob = curve (-72.54,22.332) [Pull (-66.27,33.784) (-59.05,17.236),Pull (-53.53,12.590) (-54.25,7.9437),Pull (-59.62,-2.874) (-70.74,3.7470),Pull (-81.62,0.8459) (-82.73,6.7447),Pull (-84.37,10.331) (-79.13,14.238),Pull (-78.98,22.025) (-71.94,22.932)]

brushL = curve (-87.23,11.241) [Pull (-89.48,8.5727) (-86.93,7.3442),Pull (-37.32,4.0468) (12.290,0.7494),Pull (25.629,2.5480) (38.969,4.3466),Pull (39.919,9.1428) (39.269,13.939),Pull (27.278,15.737) (15.288,17.536),Pull (14.988,9.4426) (14.688,1.3489),Pull (14.838,9.5925) (14.988,17.836),Pull (-36.42,14.388) (-87.83,10.941)]
brushR = curve (40.768,9.7423) [Pull (42.953,17.779) (51.859,18.135),Pull (65.819,8.9570) (76.740,15.138),Pull (70.848,1.2440) (54.557,0.1498),Pull (41.182,0.7061) (40.768,9.7423)]

coloursL : List Color
coloursL = [red, pink, orange, yellow, green]

coloursR : List Color
coloursR = [white, (rgb 135 206 235), blue, purple, black]

noColour = (rgb 1 1 1)

width = 30
height = 16
screenWidth = 130
screenHeight = 75

ghostPic = [(-15, -5),(-15, -4),(-15, -3),(-15, -2),(-15, -1),(-15, 0),(-15, 1),(-15, 2),(-14, -6),(-14, -5),(-14, 2),(-14, 3),(-14, 4),(-14, 5),(-13, -7),(-13, -6),(-13, 5),(-13, 6),(-12, -8),(-12, -7),(-12, 6),(-12, 7),(-11, -8),(-11, 7),(-10, -8),(-10, 7),(-10, 8),(-9, -8),(-9, 8),(-8, -8),(-8, 2),(-8, 3),(-8, 4),(-8, 5),(-8, 8),(-7, -8),(-7, 2),(-7, 5),(-7, 8),(-6, -8),(-6, 2),(-6, 5),(-6, 8),(-5, -8),(-5, 2),(-5, 5),(-5, 8),(-4, -8),(-4, 2),(-4, 3),(-4, 4),(-4, 8),(-3, -8),(-3, 2),(-3, 3),(-3, 8),(-2, -8),(-2, 8),(-1, -8),(-1, 8),(0, -8),(0, 8),(1, -8),(1, -5),(1, -4),(1, -3),(1, 8),(2, -8),(2, -5),(2, -3),(2, -2),(2, 8),(3, -8),(3, -5),(3, -2),(3, 1),(3, 2),(3, 3),(3, 4),(3, 5),(3, 8),(4, -8),(4, -5),(4, -2),(4, 1),(4, 2),(4, 3),(4, 4),(4, 5),(4, 8),(5, -8),(5, -5),(5, -2),(5, 8),(6, -8),(6, -5),(6, -4),(6, -2),(6, 8),(7, -8),(7, -4),(7, -3),(7, -2),(7, 1),(7, 2),(7, 3),(7, 4),(7, 5),(7, 8),(8, -8),(8, 1),(8, 2),(8, 3),(8, 4),(8, 5),(8, 8),(9, -8),(9, -7),(9, 8),(10, -7),(10, -6),(10, 7),(10, 8),(11, -6),(11, -5),(11, 6),(11, 7),(12, -5),(12, -4),(12, -3),(12, 3),(12, 4),(12, 5),(13, -3),(13, -2),(13, -1),(13, 0),(13, 1),(13, 2),(13, 3)]
smilePic = [(-9, -2),(-8, -3),(-7, -4),(-6, -5),(-5, -5),(-5, 2),(-5, 3),(-5, 4),(-5, 5),(-4, -5),(-4, 1),(-4, 2),(-3, -5),(-2, -5),(-1, -5),(0, -5),(1, -5),(2, -5),(2, 2),(2, 3),(2, 4),(2, 5),(3, -5),(3, 1),(3, 2),(4, -4),(5, -4),(5, -3),(6, -3),(7, -2),(8, -1),(9, 0)]
heartPic = [(-9, 0),(-9, 1),(-9, 2),(-9, 3),(-9, 4),(-8, -1),(-8, 5),(-7, -2),(-7, 6),(-6, -3),(-6, 7),(-5, -4),(-5, 7),(-4, -5),(-4, 6),(-3, -6),(-3, 5),(-2, -7),(-2, 4),(-1, -8),(-1, 3),(0, -8),(0, 2),(1, -8),(1, 3),(2, -7),(2, 4),(3, -6),(3, 5),(4, -5),(4, 6),(5, -4),(5, 7),(6, -3),(6, 7),(7, -2),(7, 6),(8, -1),(8, 5),(9, 0),(9, 1),(9, 2),(9, 3),(9, 4)]
flowerPic = [(-7, 7),(-7, 8),(-6, 6),(-6, 8),(-5, 3),(-5, 4),(-5, 5),(-5, 8),(-4, 3),(-4, 8),(-3, 2),(-3, 3),(-3, 6),(-3, 7),(-3, 8),(-2, 0),(-2, 1),(-2, 2),(-2, 6),(-1, 0),(-1, 6),(-1, 7),(-1, 8),(0, -8),(0, -7),(0, -6),(0, -5),(0, -4),(0, -3),(0, -2),(0, -1),(0, 0),(0, 8),(1, -7),(1, -6),(1, -5),(1, 0),(1, 8),(2, 0),(2, 1),(2, 2),(2, 6),(2, 7),(2, 8),(3, 2),(3, 6),(4, 2),(4, 3),(4, 6),(4, 7),(4, 8),(5, 3),(5, 4),(5, 8),(6, 5),(6, 6),(6, 7),(6, 8)]
turtlePic = [(-5, -2),(-4, -4),(-4, -3),(-4, -2),(-4, -1),(-4, 0),(-3, -4),(-3, -3),(-3, -2),(-3, -1),(-3, 0),(-3, 1),(-2, -3),(-2, -2),(-2, -1),(-2, 0),(-2, 1),(-2, 2),(-1, -3),(-1, -2),(-1, -1),(-1, 0),(-1, 1),(-1, 2),(0, -3),(0, -2),(0, -1),(0, 0),(0, 1),(0, 2),(1, -3),(1, -2),(1, -1),(1, 0),(1, 1),(2, -3),(2, -2),(2, -1),(2, 0),(3, -4),(3, -3),(3, -2),(3, -1),(3, 0),(3, 1),(4, -4),(4, -3),(4, -2),(4, -1),(4, 0),(4, 1),(4, 2),(5, -2),(5, -1),(5, 0),(5, 1),(5, 2),(5, 3),(6, -1),(6, 0),(6, 1),(6, 2),(6, 3),(7, 0),(7, 1),(7, 2),(8, 1)]

button model t noti = [ roundedRect 30 12 2
                         |> filled (hsl (degrees 5) 0.26 0.418)
                         |> addOutline (solid 1) (if model.buttonHover == t then white
                                                    else black)
                   ,  text t 
                         |> centered
                         |> filled black
                         |> addOutline (solid 0.5) black
                         |> scale 0.7
                         |> move (0, -3)
                   ] |> group |> scale 0.8
                              |> notifyTap noti
                              |> notifyEnter (MouseButtonHover t)
                              |> notifyLeave (MouseExit)
                             

---------------------------------- Calculations ----------------------------------

chooseIndex model x =
    let maxIndex = (List.length model.outlines) - 1
        nextIndex = model.outlineIndex + x
    in
    if (x /= 1)  && (x /= -1) then model.outlineIndex
    else 
      (if nextIndex > maxIndex then 0
       else if nextIndex < 0 then maxIndex
       else nextIndex)
       
checkIfFinished model =
    let keys = Dict.keys model.tiles
        input = List.sort keys
        desiredInput = List.sort (getOutline model.outlines model.outlineIndex)
    in
    listContains desiredInput input
    
listContains list1 list2 =
    case list1 of
      (x::xs) -> if (List.member x list2) then listContains xs list2
                 else False
      (_) -> True

--------- Game Model ---------

type GameState = Ongoing
               | End

type Msg = Tick Float GetKeyState
         | PickColour Color
         | MouseHover Color
         | MouseButtonHover String
         | MouseExit
         | PaintTile (Int,Int) Color
         | StartDrawing
         | StopDrawing
         | Toggle
         | NextOutline Int
         | Clear
         | FinishPainting
         | Exit

type alias Model = { time : Float 
                   , colour : Color -- Current selected colour
                   , hover : Color -- Which colour button are you hovering over with?
                   , buttonHover : String -- Which button are you hovering over with?
                   , drawing : Bool -- Is the user holding down their mouse
                   , tiles : Dict (Int, Int) Color -- Dictionary of all tiles and the colour painted
                   , outline : Bool -- The outline of what user needs to draw
                   , outlines : List (Int, Int) -- A list of all the outlines
                   , outlineIndex : Int -- Which outline are they current on
                   , gameState : GameState -- Start, Ongoing, End
                   , exit : Bool -- Exit the game or not
                   }

update msg model = case msg of
                     Tick t _ -> { model | time = t }
                     PickColour c -> { model | colour = c }
                     MouseHover c -> { model | hover = c }
                     MouseButtonHover s -> { model | buttonHover = s }
                     MouseExit -> { model | hover = noColour, buttonHover = "" }
                     PaintTile (x,y) c -> (if model.gameState == Ongoing then
                                               { model | tiles = paintTile (x,y) c model.tiles } 
                                           else
                                               model
                                          )
                     StartDrawing -> { model | drawing = True }
                     StopDrawing -> { model | drawing = False }
                     Toggle -> { model | outline = not model.outline }
                     NextOutline x -> { model | outlineIndex = (chooseIndex model x) }
                     Clear -> { model | tiles = Dict.empty }
                     FinishPainting -> (if (checkIfFinished model) then
                                         { model | gameState = End}
                                        else 
                                           model
                                        )
                     Exit -> { model | exit = True }
init = { time = 0 
       , colour = red 
       , hover = noColour 
       , buttonHover = ""
       , drawing = False
       , tiles = Dict.empty
       , outline = True
       , outlines = [ heartPic, flowerPic, turtlePic, ghostPic, smilePic ]
       , outlineIndex = 0
       , gameState = Ongoing
       , exit = False
       }
            
main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

----------------------------------------------------------------------------------
paintBrush = 
    [ curve (12.118,-35.59) [Pull (19.159,-18.93) (29.159,-2.272),Pull (36.165,9.3220) (43.171,18.556),Pull (49.798,26.790) (56.426,33.704),Pull (57.751,34.982) (59.076,34.461),Pull (60.106,33.136) (59.455,31.810),Pull (56.577,23.289) (52.260,14.769),Pull (49.082,7.7633) (45.065,0.7573),Pull (41.848,-5.491) (37.112,-11.73),Pull (34.184,-16.28) (30.295,-20.82),Pull (26.389,-27.02) (16.284,-38.62),Pull (14.769,-39.13) (13.254,-38.24),Pull (12.857,-38.01) (11.739,-36.73),Pull (11.739,-36.16) (12.118,-35.59)]
        |> filled purple
        |> addOutline (solid 4) black 
    , curve (11.863,-36.83) [Pull (7.6487,-37.00) (3.4341,-41.52),Pull (0.6048,-45.63) (-0.624,-50.26),Pull (-2.723,-55.56) (-10.30,-59.62),Pull (-4.058,-61.66) (2.1853,-60.25),Pull (9.3804,-57.20) (12.175,-53.07),Pull (15.773,-49.76) (17.170,-45.58),Pull (18.174,-42.30) (16.858,-39.02),Pull (16.702,-39.18) (16.546,-39.33),Pull (15.453,-39.83) (14.360,-39.33),Pull (12.392,-38.78) (11.863,-36.83)]
        |> filled brown
        |> addOutline (solid 4) black
    , curve (18.731,-22.16) [Pull (22.321,-24.50) (25.912,-26.84),Pull (21.229,-32.93) (16.546,-39.02),Pull (15.361,-39.52) (13.736,-39.02),Pull (12.540,-38.47) (11.863,-36.52),Pull (15.297,-29.34) (18.731,-22.16)]
        |> filled yellow
        |> addOutline (solid 1) black
        |> scale 1.1
        |> move (-1,4.5)
    , curve (2.1853,-43.39) [Pull (1.9819,-45.58) (4.0585,-47.76),Pull (4.8390,-48.43) (5.6195,-48.70),Pull (7.3604,-48.79) (8.7414,-47.76),Pull (9.9902,-47.09) (11.239,-47.14),Pull (12.019,-47.26) (12.8,-48.07),Pull (13.784,-49.63) (14.048,-51.2),Pull (14.204,-51.2) (14.360,-51.2),Pull (11.070,-55.34) (5.6195,-59.00),Pull (3.2780,-60.36) (0.9365,-60.56),Pull (-1.717,-61.06) (-4.370,-60.87),Pull (-7.180,-60.52) (-9.990,-59.94),Pull (-6.704,-58.09) (-4.058,-55.57),Pull (-1.941,-52.91) (-0.624,-50.26),Pull (0.2643,-46.82) (2.1853,-43.39)]
        |> filled green
        |> addOutline (solid 2) black
    ] |> group |> scale 0.2 


palette = 
    [ curve (9.3658,10.926) [Pull (8.3180,15.453) (9.9902,19.980),Pull (10.998,23.570) (10.926,27.160),Pull (10.417,31.299) (6.8682,35.278),Pull (3.2780,38.050) (-0.312,37.463),Pull (-8.429,37.905) (-16.54,32.468),Pull (-28.70,22.905) (-32.46,11.863),Pull (-37.76,0.7804) (-37.46,-10.30),Pull (-36.89,-17.01) (-33.71,-23.72),Pull (-28.94,-31.14) (-21.85,-33.71),Pull (-14.16,-37.08) (-6.243,-36.52),Pull (5.2219,-34.51) (12.487,-26.53),Pull (17.014,-22.00) (19.980,-17.48),Pull (23.438,-10.45) (24.975,-3.434),Pull (25.655,1.2487) (23.414,5.9317),Pull (22.541,7.6687) (21.229,9.3658),Pull (20.124,10.274) (18.419,10.302),Pull (15.297,10.770) (12.175,9.6780),Pull (10.926,9.1741) (9.6780,9.9902),Pull (9.3619,10.458) (9.3658,10.926)]
        |> filled (hsl (degrees 5) 0.26 0.418)
        |> addOutline (solid 4) black
    , oval 14 17
        |> filled (hsl (degrees 273) 0.139 0.731)
        |> addOutline (solid 3) black
        |> move (-2,5)
        |> scale 0.9
    , curve (-3.595,-22.29) [Pull (-4.494,-22.09) (-5.393,-23.37),Pull (-5.492,-24.08) (-6.471,-24.80),Pull (-7.370,-24.98) (-8.269,-26.24),Pull (-8.370,-27.68) (-7.191,-29.12),Pull (-6.112,-29.64) (-5.033,-29.12),Pull (-4.314,-28.62) (-3.595,-28.76),Pull (-2.755,-29.12) (-2.876,-29.48),Pull (-2.157,-29.74) (-1.438,-29.12),Pull (-0.539,-27.88) (0.3595,-26.96),Pull (1.8186,-26.06) (1.7977,-25.16),Pull (2.1984,-24.26) (0.7191,-23.37),Pull (-0.719,-23.21) (-2.157,-22.65),Pull (-2.876,-22.47) (-3.595,-22.29)]
        |> filled yellow
        |> addOutline (solid 1) black
        |> move (0,5)
    , curve (-3.595,-22.29) [Pull (-4.494,-22.09) (-5.393,-23.37),Pull (-5.492,-24.08) (-6.471,-24.80),Pull (-7.370,-24.98) (-8.269,-26.24),Pull (-8.370,-27.68) (-7.191,-29.12),Pull (-6.112,-29.64) (-5.033,-29.12),Pull (-4.314,-28.62) (-3.595,-28.76),Pull (-2.755,-29.12) (-2.876,-29.48),Pull (-2.157,-29.74) (-1.438,-29.12),Pull (-0.539,-27.88) (0.3595,-26.96),Pull (1.8186,-26.06) (1.7977,-25.16),Pull (2.1984,-24.26) (0.7191,-23.37),Pull (-0.719,-23.21) (-2.157,-22.65),Pull (-2.876,-22.47) (-3.595,-22.29)]
        |> filled yellow
        |> addOutline (solid 1) black
        |> move (15,15)
    , curve (-3.595,-22.29) [Pull (-4.494,-22.09) (-5.393,-23.37),Pull (-5.492,-24.08) (-6.471,-24.80),Pull (-7.370,-24.98) (-8.269,-26.24),Pull (-8.370,-27.68) (-7.191,-29.12),Pull (-6.112,-29.64) (-5.033,-29.12),Pull (-4.314,-28.62) (-3.595,-28.76),Pull (-2.755,-29.12) (-2.876,-29.48),Pull (-2.157,-29.74) (-1.438,-29.12),Pull (-0.539,-27.88) (0.3595,-26.96),Pull (1.8186,-26.06) (1.7977,-25.16),Pull (2.1984,-24.26) (0.7191,-23.37),Pull (-0.719,-23.21) (-2.157,-22.65),Pull (-2.876,-22.47) (-3.595,-22.29)]
        |> filled red
        |> addOutline (solid 1) black
        |> move (15,15)
    , curve (-3.595,-22.29) [Pull (-4.494,-22.09) (-5.393,-23.37),Pull (-5.492,-24.08) (-6.471,-24.80),Pull (-7.370,-24.98) (-8.269,-26.24),Pull (-8.370,-27.68) (-7.191,-29.12),Pull (-6.112,-29.64) (-5.033,-29.12),Pull (-4.314,-28.62) (-3.595,-28.76),Pull (-2.755,-29.12) (-2.876,-29.48),Pull (-2.157,-29.74) (-1.438,-29.12),Pull (-0.539,-27.88) (0.3595,-26.96),Pull (1.8186,-26.06) (1.7977,-25.16),Pull (2.1984,-24.26) (0.7191,-23.37),Pull (-0.719,-23.21) (-2.157,-22.65),Pull (-2.876,-22.47) (-3.595,-22.29)]
        |> filled green
        |> addOutline (solid 1) black
        |> move (-17,10)
    , curve (-3.595,-22.29) [Pull (-4.494,-22.09) (-5.393,-23.37),Pull (-5.492,-24.08) (-6.471,-24.80),Pull (-7.370,-24.98) (-8.269,-26.24),Pull (-8.370,-27.68) (-7.191,-29.12),Pull (-6.112,-29.64) (-5.033,-29.12),Pull (-4.314,-28.62) (-3.595,-28.76),Pull (-2.755,-29.12) (-2.876,-29.48),Pull (-2.157,-29.74) (-1.438,-29.12),Pull (-0.539,-27.88) (0.3595,-26.96),Pull (1.8186,-26.06) (1.7977,-25.16),Pull (2.1984,-24.26) (0.7191,-23.37),Pull (-0.719,-23.21) (-2.157,-22.65),Pull (-2.876,-22.47) (-3.595,-22.29)]
        |> filled purple
        |> addOutline (solid 1) black
        |> move (-20,25)
    , curve (-3.595,-22.29) [Pull (-4.494,-22.09) (-5.393,-23.37),Pull (-5.492,-24.08) (-6.471,-24.80),Pull (-7.370,-24.98) (-8.269,-26.24),Pull (-8.370,-27.68) (-7.191,-29.12),Pull (-6.112,-29.64) (-5.033,-29.12),Pull (-4.314,-28.62) (-3.595,-28.76),Pull (-2.755,-29.12) (-2.876,-29.48),Pull (-2.157,-29.74) (-1.438,-29.12),Pull (-0.539,-27.88) (0.3595,-26.96),Pull (1.8186,-26.06) (1.7977,-25.16),Pull (2.1984,-24.26) (0.7191,-23.37),Pull (-0.719,-23.21) (-2.157,-22.65),Pull (-2.876,-22.47) (-3.595,-22.29)]
        |> filled blue
        |> addOutline (solid 1) black
        |> move (-15,42)
    ] |> group |> scale 0.3
    
    
paintingText = 
    [ text " Painting! "
        |> bold
        |> italic
        |> filled purple
        |> addOutline (solid 0.7) (hsl (degrees 279) 0.145 0.264)
        |> scale 1
        |> move (-25,47) ]
    |> group
 
paintBucket c model = 
    [ curve (-22.48,14.238) [Pull (-15.11,19.656) (-4.196,20.234),Pull (6.3063,20.986) (13.489,13.939),Pull (7.5357,8.3414) (-2.697,7.9437),Pull (-13.42,6.7513) (-22.48,14.238)]
         |> filled (if c == black then (hsl (degrees 0) 0.026 0.357) else black)
         -- |> addOutline (solid 2) black 
         |> addOutline (solid 2)
                              (if model.colour == c then black
                               else if model.hover == c then gray
                               else black)
    , curve (-22.48,13.039) [Pull (-22.63,-0.899) (-22.78,-14.83),Pull (-13.71,-21.19) (-3.297,-20.23),Pull (6.3656,-20.61) (14.388,-14.83),Pull (14.088,-0.899) (13.789,13.039),Pull (5.0960,8.0216) (-3.597,8.2435),Pull (-13.03,7.5515) (-22.48,13.039)]
         |> filled c
         -- |> addOutline (solid 2) black
         |> addOutline (solid 2)
                              (if model.colour == c then black
                               else if model.hover == c then gray
                               else black)
         |> scale 0.95
         |> move (0,1)
    , curve (-23.38,9.4426) [Pull (-24.88,9.8922) (-26.37,10.341),Pull (-26.58,32.421) (-5.395,35.822),Pull (17.465,34.391) (17.686,11.241),Pull (16.487,9.8922) (15.288,8.5433)]
         -- |> outlined (solid 3) black
         |> outlined (solid 3)
                              (if model.colour == c then black
                               else if model.hover == c then gray
                               else black)
         |> scale 0.9
         |> move (-0.2,1)
    ] |> group |> scale 0.27    

hexagon = 
    [ ngon 5 10
        |> outlined (dashed 1) yellow
        |> move (-160,115)
        |> scale 0.5
    , ngon 5 10
        |> outlined (dashed 1) yellow
        |> move (160,115)
        |> scale 0.5
    , ngon 5 10
        |> outlined (dashed 1) purple 
        |> move (-170,115)
        |> scale 0.5
    , ngon 5 7
        |> filled purple 
        |> move (-170,115)
        |> scale 0.5
    , ngon 5 10
        |> outlined (dashed 1) purple 
        |> move (170,115)
        |> scale 0.5
    , ngon 5 7
        |> filled purple 
        |> move (170,115)
        |> scale 0.5
    ] 
    |> group

