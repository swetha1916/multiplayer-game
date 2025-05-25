module Wordle exposing (..)

testwords: List String
testwords = ["FUZZY","JAZZY","EXTRA","CABLE","CABIN", "QUICK"
            , "FUNNY", "HAPPY", "CRAZY","FOODY", "FUNNY", "GOOFY",
            "MUSIC", "SHADY", "SHORT", "OASIS", "PACES", "RABID",
            "VAGUE", "ENEMY", "EXTRA", "ABACK", "ABORT", "ITEMS",
            "ISSUE", "INNER", "INPUT", "EASED", "LEAKS", "LEARN",
            "LEASE"]
            
myShapes model =
  (case model.mode of
      Light -> [ text "Wordle"
                    |> bold
                    |> filled (rgb 120 124 127)
                    |> addOutline (solid 0.8) black
                    |> scale 0.9
                    |> move (-18,53) 
               , darkModeSwitch
                    |> notifyTap ChangeBack ]
      Dark -> [ square 200
                    |> filled (hsl (degrees 241) 0.032 0.164)
              , text "Wordle"
                    |> bold
                    |> filled white
                    |> addOutline (solid 0.2) white
                    |> scale 0.9
                    |> move (-18,53) 
               , whiteModeSwitch
                    |> notifyTap ChangeBack ]
      End -> [
                group
                 [
                 square 200
                   |> filled (hsl (degrees 241) 0.032 0.164)
                 , rightBlocks Dark
                 , leftBlocks Dark
                 , roundedRect 100 60 2
                    |> filled (rgb 120 124 127)
                    |> addOutline (solid 1) (rgb 237 237 237) 


                 , text "Wordle"
                    |> bold
                    |> filled white
                    |> addOutline (solid 0.2) white
                    |> scale 0.9
                    |> move (-18,50)

                  , playAgain 
                    |> notifyTap PlayAgain
                    |> notifyEnter Hovering
                    |> notifyLeave UnHovering

                  , checkwinloss (updatewinloss (firstword model.words) model.checkstrings)
                  ,  playAgain 
                        |> notifyTap PlayAgain 
                 ]
              ]
  )      
  ++
  if model.mode == Light || model.mode == Dark
  then 
  [ 
  wordleBlocks
  , 
  [ 
  [ displayfunc (String.toList "QWERTYUIOP") "white"
      |> move (-70,-46)
  , displayfunc (String.toList "ASDFGHJKL") "white"
      |> move (-62,-61)
  , displayfunc (String.toList "ZXCVBNM") "white"
      |> move (-47,-76) ] |> group |> scale 0.8
  , 
  [ roundedRect 17 10 2
               |> filled (rgb 237 237 240)
               |> addOutline (solid 0.5) grey
               |> scale 1.3
               |> move (-62,-72)
           , text "ENTER"
               |> bold
               |> centered
               |> sansserif
               |> filled black
               |> scale 0.45
               |> move (-62,-74) ] |> group |> scale 0.8
           |> notifyTap Enter
           , 
           [ roundedRect 17 10 2
               |> filled (rgb 237 237 240)
               |> addOutline (solid 0.5) grey
               |> scale 1.3
               |> move (60,-72)
           , curve (5.3957,3.4473) [Pull (0.4496,3.2974) (-4.496,3.1475),Pull (-5.695,0.7494) (-6.894,-1.648),Pull (-5.395,-2.847) (-3.896,-4.046),Pull (0.7494,-4.046) (5.3957,-4.046),Pull (5.2459,-0.299) (5.3957,3.4473)]
               |> outlined (solid 1) black
               |> scale 0.8
               |> move (60,-72)
           , text "x"
               |> bold
               |> centered
               |> sansserif
               |> filled black
               |> scale 0.45
               |> move (60,-73.8) ] |> group |> scale 0.8
           |> notifyTap Back
  ] |> group
  
  , text (lightordark model.mode)
       |> sansserif
       |> italic
       |> bold
       |> filled (checkcol model.col)
       |> scale 0.3
       |> move (38,49)
  , rightBlocks model.mode
  , leftBlocks model.mode
  ,
  [ roundedRect 40 10 3 
      |> filled (if model.mode == Light then grey else (rgb 120 124 127))
      |> move (-70,1.2)
  , text "Type the word!"
      |> sansserif
      |> italic
      |> bold
      |> filled (if model.mode == Light then black else white)
      |> scale 0.4
      |> move (-87,0) ] |> group 
  , text model.string  
      |> sansserif
      |> filled (checkcol model.col)
      |> move (-110,-20)
      |> scale 0.8 
   , 
   [ attemptsSign model.mode
      |> move (-50,58)  
   , text ("Attempts")
      |> bold
      |> italic
      |> sansserif
      |> filled (checkcol model.col)
      |> scale 0.32
      |> move (-57,48)
   , text (String.fromInt model.attempts)
      |> bold
      |> italic
      |> sansserif
      |> filled (checkcol model.col)
      |> scale 0.35
      |> move (-50.2,55.2) ] |> group
     
   , displaystrings (firstword model.words) model.checkstrings model.mode
   , 
   [ roundedRect 25 10 3 
      |> filled (if model.mode == Light then grey else (rgb 120 124 127))
      |> move (70,1.2)
   , text "Hints!"
      |> sansserif
      |> italic
      |> bold
      |> filled (if model.mode == Light then black else white)
      |> scale 0.4
      |> move (63,0) ] |> group 
           
    , group [
        if (model.attempts == 2) then
          group [
              text ((String.left 1 (String.dropLeft 2 (firstword model.words))) ++ " is in the 3rd")
                |> sansserif
                |> bold
                |> italic
                |> centered
                |> filled (rgb 200 182 83)
                |> scale 0.8
                |> move (280,-10)
              , text "position"
                |> sansserif
                |> bold
                |> italic
                |> centered
                |> filled (rgb 200 182 83)
                |> scale 0.8
                |> move (280,-20) ] 
                    |> move (3,20)
        else if (model.attempts == 4) then
          group [
              text ((String.left 1 (firstword model.words)) ++ " is in the 1st")
                |> sansserif
                |> bold
                |> italic
                |> centered
                |> filled (rgb 200 182 83)
                |> scale 0.8
                |> move (280,-10)
              , text "position"
                |> sansserif
                |> bold
                |> italic
                |> centered
                |> filled (rgb 200 182 83)
                |> scale 0.8
                |> move (280,-20) 
              , text ((String.right 1 (firstword model.words)) ++ " is in the 5th")
                |> sansserif
                |> bold
                |> italic
                |> centered
                |> filled (rgb 108 169 101)
                |> scale 0.8
                |> move (280,-30)
              , text "position"
                |> sansserif
                |> bold
                |> italic
                |> centered
                |> filled (rgb 108 169 101)
                |> scale 0.8
                |> move (280,-40) ] 
                    |> move (7,13)
        else
            circle 0.00000000000001
              |> filled white ] 
                  |> move (-145,-30)
                  |> scale 0.5
              
   , if (model.attempts == 6 || updatewinloss (firstword model.words) model.checkstrings == True) then
     (
       group
       [
       roundedRect 60 16 5
           |> filled blue
           |> (if model.over == True then addOutline (solid 1) black else identity)
           |> move (0,5)
       , text "Click here!"
         |> centered
         |> filled black
         |> notifyTap EndGame
       ]
         |> notifyEnter Hovering
         |> notifyLeave UnHovering
         
     )
     else 
     circle 0.000000000001
       |> filled black
       
       
    , text "Exit"
        |> filled (if model.col == "black" then white else black)
        |> move (-90,-60) 
        |> notifyTap Exit
  ]
  else
  [
  ] 

----------------------------------------------------------------------------------
type Msg = Tick Float GetKeyState | ChangeBack 
           | Typedkey String | Back | Enter | EndGame
           | PlayAgain | Hovering | UnHovering | Exit
           
 
type Mode = Light | Dark | End


update msg model = case msg of             
                     Tick t _ -> {model | time = t}
                     ChangeBack -> {model | mode = (if model.mode == Light then Dark else Light)  
                                           , col = (if model.mode == Light then "black" else "gray") }
                     Typedkey s -> {model | string = if (String.length model.string < 5) 
                                                     then model.string ++ s
                                                     else model.string}
                     Back -> {model | string = String.dropRight 1 model.string}
                     Enter -> {model | attempts =  if (model.attempts < 6) 
                                                then model.attempts + 1
                                                else model.attempts
                                       , string  = if (model.attempts < 6)
                                                  then ""
                                                  else model.string
                                       , checkstrings = if (model.attempts < 6) then model.checkstrings ++ [model.string] else model.checkstrings
                                       , val = True              
                               } 
                     EndGame -> {model | mode = End}
                     PlayAgain -> {model | mode = Light, col = "white", attempts = 0, checkstrings = [], words = List.drop 1 model.words, val = False, over = False, coins = if (updatewinloss (firstword model.words) model.checkstrings == True) then model.coins + 10 else model.coins}
                     Hovering -> {model | over = True}
                     UnHovering -> {model | over = False}
                     Exit -> {model | exit = True}
                     
type alias Model = { time : Float , mode: Mode , col : String, attempts: Int, string: String
                    , checkstrings: List String, words: List String, val: Bool, over : Bool, exit: Bool, coins: Int}

init = { time = 0 , mode = Light, col = "white", attempts = 0, string = "", checkstrings = [], words = testwords, val = False, over = False, exit = False, coins = 0}

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)

------------------------------------------------------------------------------

darkModeSwitch = 
    [ roundedRect 13 7 4
        |> filled gray
        |> addOutline (solid 0.7) (rgb 120 124 127)
    , circle 2.7
        |> filled white
        |> addOutline (solid 0.2) (rgb 120 124 125)
        |> move (-3,0) ]
    |> group |> move (50,57)


whiteModeSwitch = 
    [ roundedRect 13 7 4
        |> filled (hsl (degrees 241) 0.02 0.468)
        |> addOutline (solid 0.7) grey
    , circle 2.7
        |> filled black
        |> addOutline (solid 0.2) (rgb 120 124 125)
        |> move (3,0) ]
    |> group |> move (50,57)


squares x = 
    if x > 0 then 
        [ square 13
            |> outlined (solid 1) grey
        , squares (x - 1) 
            |> move (16,0) 
        ] |> group
    else 
        [] |> group
        
        
wordleBlocks = 
    [ squares 5
      |> move (-35,50)
    , squares 5
      |> move (-35,34)
    , squares 5 
      |> move (-35,18)
    , squares 5
      |> move (-35,2)
    , squares 5 
      |> move (-35,-14)
    , squares 5 
      |> move (-35,-30)
    ] 
    |> group
    |> move (3,5)
    |> scale 0.8


displayfunc ltrs coll = 
  case ltrs of
      p1::p2 -> [ group 
                    [ roundedRect 9 10 2
                        |> filled (rgb 237 237 240)
                        |> addOutline (solid 0.5) grey
                        |> scale 1.3
                        |> move (4,4)
                    , text (String.fromChar p1)
                        |> bold
                        |> centered
                        |> sansserif
                        |> filled (if coll == "black" then white else black)
                        |> scale 0.6
                        |> move (4,1)
                     ] |> notifyTap (Typedkey (String.fromChar p1))
                     , displayfunc p2 coll |> move (14,0) 
                ] |> group
      _ -> [ text ""
               |> filled black ] 
           |> group


box clr pt clr2 = 
    [ rect 10 10
        |> filled clr
        |> addOutline (solid 1.2) clr2
        |> move pt 
    ]
    |> group
    


leftBlocks mode = 
    [ box (rgb 120 124 127) (-90,55) black -- grey
    , box (rgb 200 182 83) (-78,55) black -- yellow
    , box (rgb 200 182 83) (-66,55) black
    , box (rgb 108 169 101) (-54,55) black -- green
    , box (rgb 120 124 127) (-90,43) black
    , box (rgb 200 182 83) (-78,43) black
    , box (rgb 108 169 101) (-66,43) black
    , box (rgb 120 124 127) (-90,31) black
    , box (rgb 108 169 101) (-78,31) black
    , box (rgb 108 169 101) (-78,31) black
    , box (rgb 108 169 101) (-90,19) black
    , box (rgb 108 169 101) (-90,-2) black
    , box (rgb 108 169 101) (-90,-26) black
    , box (if mode == Light then (rgb 237 237 237) else black) (-42,55) (if mode == Light then (rgb 237 237 237) else (hsl (degrees 241) 0.032 0.164))
    , box (if mode == Light then (rgb 237 237 237) else black) (-54,43) (if mode == Light then (rgb 237 237 237) else (hsl (degrees 241) 0.032 0.164))
    , box (if mode == Light then (rgb 237 237 237) else black) (-66,31) (if mode == Light then (rgb 237 237 237) else (hsl (degrees 241) 0.032 0.164))
    , box (if mode == Light then (rgb 237 237 237) else black) (-78,19) (if mode == Light then (rgb 237 237 237) else (hsl (degrees 241) 0.032 0.164)) ]
    |> group |> scale 0.5 |> move (-40,30)
      
      
rightBlocks mode = 
    [ box (rgb 120 124 127) (90,55) black -- grey
    , box (rgb 200 182 83) (78,55) black -- yellow
    , box (rgb 200 182 83) (66,55) black
    , box (rgb 108 169 101) (54,55) black -- green
    , box (rgb 120 124 127) (90,43) black
    , box (rgb 200 182 83) (78,43) black
    , box (rgb 108 169 101) (66,43) black
    , box (rgb 120 124 127) (90,31) black
    , box (rgb 108 169 101) (78,31) black
    , box (rgb 108 169 101) (78,31) black
    , box (rgb 108 169 101) (90,19) black
    , box (rgb 108 169 101) (90,-2) black
    , box (rgb 108 169 101) (90,-26) black
    , box (if mode == Light then (rgb 237 237 237) else black) (42,55) (if mode == Light then (rgb 237 237 237) else (hsl (degrees 241) 0.032 0.164))
    , box (if mode == Light then (rgb 237 237 237) else black) (54,43) (if mode == Light then (rgb 237 237 237) else (hsl (degrees 241) 0.032 0.164))
    , box (if mode == Light then (rgb 237 237 237) else black) (66,31) (if mode == Light then (rgb 237 237 237) else (hsl (degrees 241) 0.032 0.164))
    , box (if mode == Light then (rgb 237 237 237) else black) (78,19) (if mode == Light then (rgb 237 237 237) else (hsl (degrees 241) 0.032 0.164)) ]
    |> group |> scale 0.5 |> move (40,30)


attemptsSign mode = 
    [ curve (13.189,11.540) [Pull (4.6463,15.811) (-3.896,11.241),Pull (-11.58,5.5661) (-12.59,-1.348),Pull (-14.15,-11.19) (-8.393,-17.23),Pull (-2.209,-24.32) (8.0936,-24.13),Pull (17.378,-21.67) (21.583,-14.53),Pull (26.012,-6.894) (23.081,0.7494),Pull (25.779,1.7985) (28.477,2.8477),Pull (32.077,-4.496) (29.676,-11.84),Pull (27.051,-21.93) (16.786,-27.42),Pull (8.0244,-32.16) (-2.697,-29.22),Pull (-11.27,-25.71) (-17.08,-17.23),Pull (-21.44,-7.924) (-18.28,1.9484),Pull (-16.21,8.6644) (-11.99,12.140),Pull (-6.026,17.857) (2.6978,19.934),Pull (11.012,20.155) (17.086,16.337),Pull (18.135,17.536) (19.185,18.735),Pull (21.433,12.590) (23.681,6.4449),Pull (17.386,7.7939) (11.091,9.1428),Pull (12.290,10.341) (13.189,11.540)]
        |> filled (if mode == Light then (rgb 237 237 237) else black)
        |> addOutline (solid 3) (if mode == Light then (rgb 120 124 127) else (rgb 237 237 237)) ]
    |> group |> scale 0.2


displaystrings checkstr strs mode = 
    case strs of
        x1::x2 -> [ displaygrnsqrs checkstr x1 mode |> scale 0.8 |> move (-25.6,44)
                  , displaystrings checkstr x2 mode |> move (0,-12.7) ] |> group
        _ -> [] |> group  
        
        
checkmatch correctstring checkstring = 
      case (correctstring,checkstring) of
          ("","") -> True
          (a1,a2) -> (String.left 1 a1 == String.left 1 a2) && (checkmatch (String.dropLeft 1 a1) (String.dropLeft 1 a2))          
          
        
displaygrnsqrs corrctstr chkstr mode = 
      case (corrctstr,chkstr) of
         ("","") -> [] |> group
         (a11,a22) -> [
                      square 13
                        |> filled (if String.left 1 a11 == String.left 1 a22 
                                   then (rgb 108 169 101)
                                   else (rgb 120 124 127))
                      , text (String.left 1 a22)
                        |> sansserif
                        |> filled (if mode == Light then black else white)
                        |> scale 0.8
                        |> move (-3.5,-3.5)
                      , displaygrnsqrs (String.dropLeft 1 a11) (String.dropLeft 1 a22) mode |> move (16,0)
                      ] |> group
                      
firstword lst = 
    case lst of
        x::xs -> x
        _ -> ""

checkcol md =
    if md == "black"
    then white
    else black
  
lightordark mde =  
    if mde == Light
    then "Light theme"
    else if mde == Dark
    then "Dark theme"
    else
    ""
  
checkwinloss x = 
    if x == True then 
      [ text "Congrats! you"
          |> sansserif
          |> bold
          |> filled (rgb 200 182 83)
          |> scale 0.5
          |> move (-35,15)
      , text "WIN!"
          |> sansserif
          |> bold
          |> italic
          |> filled (rgb 108 169 101)
          |> addOutline (solid 0.5) black
          |> scale 2 
          |> move (-20,-10) ] |> group          
    else
      [ text "Sorry, you"
          |> sansserif
          |> bold
          |> filled (rgb 200 182 83)
          |> scale 0.5
          |> move (-35,15)
      , text "LOST "
          |> sansserif
          |> bold
          |> italic
          |> filled (rgb 108 169 101)
          |> addOutline (solid 0.5) black
          |> scale 2 
          |> move (-30,-10) ] |> group
         
         
updatewinloss correctstr chkstrings = 
    case (correctstr,chkstrings) of
        (_,s1::rest) -> (correctstr == s1) || (updatewinloss correctstr rest)
        (_,_) -> False
        

playAgain =
    [ box (rgb 120 124 127) (-40,-40) black 
    , text "P" 
        |> bold
        |> filled black
        |> scale 0.7
        |> move (-43,-43) 
    , box (rgb 120 124 127) (-28,-40) black 
    , text "L" 
        |> bold
        |> filled black
        |> scale 0.7
        |> move (-31,-43)
    , box (rgb 200 182 83) (-16,-40) black 
    , text "A" 
        |> bold
        |> filled black
        |> scale 0.7
        |> move (-19,-43) 
    , box (rgb 200 182 83) (-4,-40) black 
    , text "Y" 
        |> bold
        |> filled black
        |> scale 0.7
        |> move (-7,-43) 
    , box (rgb 108 169 101) (8,-40) black 
    , text "A" 
        |> bold
        |> filled black
        |> scale 0.7
        |> move (5,-43) 
    , box (rgb 108 169 101) (20,-40) black 
    , text "G" 
        |> bold
        |> filled black
        |> scale 0.7
        |> move (17,-43) 
    , box (rgb 120 124 127) (32,-40) black 
    , text "A" 
        |> bold
        |> filled black
        |> scale 0.7
        |> move (29,-43) 
    , box (rgb 120 124 127) (44,-40) black 
    , text "I" 
        |> bold
        |> filled black
        |> scale 0.7
        |> move (42.5,-43) 
    , box (rgb 200 182 83) (56,-40) black 
    , text "N" 
        |> bold
        |> filled black
        |> scale 0.7
        |> move (53,-43) 
    ] |> group |> move (-7,0)

