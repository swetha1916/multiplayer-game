module TypingTest exposing (..)

import Random exposing(..)

myShapes model =
  -- Blinking cursor while user is typing
  let cursor = if (sin (5*model.time) < 0) && model.gameState == Ongoing then "|" else ""
  in
  [ -- Background
    bg, bg |> move (0,-60)
    -- Title Box
  , typingTest
    -- The box containing the texts
  , typeBox
  , startButton model
  , renderText (String.lines model.output) darkGrey
        |> move (-73, 30)
  , renderText (String.lines (model.input ++ cursor)) black
        |> move (-73, 30)
        
  , scoreTimeText model
  , keyboard model
        |> move (-45,-19)
    -- Display a textbox on their typo/mistake
  , (if model.showMistake then [warningBox model.mistake]
  
    -- Display accuracy and win boxes when they finish typing
    else if model.gameState == End then [ victoryBox model ]
    else [] 
    ) |> group |> move (50, -8)
  --, text (List.foldl String.append "" model.chars1 ++ cursor) |> size 8 |> filled black
  ]

------- Rendering Functions ---------
-- Render the passage the user has to type/is typing
-- Takes in a list of sentences seperated by line breaks
renderText output c =
  case (output) of
  (x::xs) ->
    [ text x
          |> sansserif
          |> bold
          |> size 5
          |> filled c
    , renderText xs c
          |> move (0, -8)
    ] |> group
  _ -> [] |> group
  
  
-- Implement a key of the keyboard
renderKey model ltr width clr =
  [
    roundedRect width 10 2
      |> filled (if model.hover == ltr then white else clr)
      |> addOutline (solid 1) black
      |> notifyTap (TypedKey ltr)
      |> notifyEnter (MouseOver ltr)
      |> notifyLeave (MouseOff)
  , text ltr
      |> sansserif
      |> bold
      |> size 5
      |> centered
      |> filled black
      |> move (0, -2)
      |> notifyTap (TypedKey ltr)
      |> notifyEnter (MouseOver ltr)
      |> notifyLeave (MouseOff)
  ] |> group

  
-- Implement a key that has a different functionality than inputing a letter
renderSpecialKey model ltr width clr input =
  [
    roundedRect width 10 2
      |> filled (if model.hover == ltr then white else clr)
      |> addOutline (solid 0.5) black
      |> notifyTap (TypedKey input)
      |> notifyEnter (MouseOver ltr)
      |> notifyLeave (MouseOff)
  , text ltr
      |> sansserif
      |> bold
      |> size 4
      |> centered
      |> filled black
      |> move (0, -1.5)
      |> notifyTap (TypedKey input)
      |> notifyEnter (MouseOver ltr)
      |> notifyLeave (MouseOff)
  ] |> group

-- Render a row of keys with an array with a colour pattern
renderRow model ltrs clrs =
  case (ltrs, clrs) of
    (l::ls, c::cs) -> [  renderKey model (String.fromChar l) 10 c
                      ,  renderRow model ls cs
                            |> move (10, 0)
                      ] |> group
    (l::ls, _) -> [  renderKey model (String.fromChar l) 10 red
                      ,  renderRow model ls []
                            |> move (10, 0)
                      ] |> group -- Make every key red after the colour scheme
    _ -> [] |> group
    
-- Render the start game and exit game button
startButton model =   
    [ roundedRect 31 11 3
        |> filled red
        |> addOutline (solid 1) (if model.hover == "Start" then white else black)
    , text (if model.gameState == Start then "START" else "EXIT")
        |> sansserif 
        |> bold
        |> filled white
        |> addOutline (solid 0.7) black
        |> scale 0.45
        |> move (-6,-2) ]
    ++
     ( if model.gameState == Start then 
         [triangle 10
            |> filled white
            |> addOutline (solid 1.5) black
            |> scale 0.3
            |> move (-11,0) ]
     else [] ) |> group |> move (-77,52) 
               |> notifyTap (if model.gameState == Start then StartTimer else Exit)
               |> notifyEnter (MouseOver "Start")
               |> notifyLeave (MouseOff)
  
--------- Predefined Values ---------

-- use these variables for your collage size
collageWidth = 192
collageHeight = 128

appTitle = "Typing Arena"

-- depending on the state you can turn on and off typing
allowTyping model = model.state /= NotTyping 

-- depending on state you can turn on and off animation (the Tick message)
isAnimating model = True

lOrange = (rgb 255 153 102)
tan = (rgb 255 255 153)

row1 = "qwertyuiop !"
row2 =  "asdfghjkl;\'"
row3 =  "zxcvbnm,./"

row1b = "QWERTYUIOP !"
row2b = "ASDFGHJKL:\""
row3b = "ZXCVBNM<>?"

colour1 = [red, lOrange, orange, yellow, tan, tan, yellow, orange, lOrange, red]
    
    
keyboard model = [
      roundedRect 159.5 50 2
        |> filled black
        |> addOutline (solid 1) white
        |> move (45, -15)
    , renderRow model (String.toList (if model.caps then row1b else row1)) colour1
        |> move (-12, 0)
    , renderSpecialKey model "TAB" 15 red " "
        |> move (-25, 0)
    , renderSpecialKey model "BACK" 20 red ""
        |> move (113, 0)
        |> notifyTap BackKey
    , renderRow model (String.toList (if model.caps then row2b else row2)) colour1
        |> move (-7.5, -10)
    , renderSpecialKey model "CAPS" 20 red ""
        |> move (-22.5, -10)
        |> notifyTap CapsToggle
    , renderSpecialKey model "ENTER" 25 red "\n"
        |> move (110.5, -10)
    , renderRow model (String.toList (if model.caps then row3b else row3)) colour1
        |> move (-2, -20)
    , renderSpecialKey model "SHIFT" 24 red ""
        |> move (-20, -20)
        |> notifyTap CapsToggle
    , renderSpecialKey model "SHIFT" 30 red ""
        |> move (108, -20)
        |> notifyTap CapsToggle
    , renderSpecialKey model "SPACE" 70 tan " "
        |> move (48, -30)
    ] |> group

-- Includes calculations for Words per Minute and timer
-- Total characters / 5 / total minutes
scoreTimeText model =
    let wordCount = toFloat (String.length model.input)
        timer = (if model.gameState == End then model.finalTime
                 else (model.time - model.startTime) )
    in
    [ roundedRect 35 16 2
        |> filled red
        |> addOutline (solid 0.5) white
    , text "WPM : "
        |> sansserif 
        |> bold
        |> filled white
        |> addOutline (solid 0.7) black
        |> scale 0.45
        |> move (-15,2)
    , text (if model.gameState == Start then "0"
            else String.fromInt (round ( (wordCount / 5) / (timer / 60) ) )
           )
        |> sansserif 
        |> bold
        |> filled white
        |> addOutline (solid 0.7) black
        |> scale 0.45
        |> move (5,2)
    , text "TIME : "
        |> sansserif 
        |> bold
        |> filled white
        |> addOutline (solid 0.7) black
        |> scale 0.45
        |> move (-15,-6)
    , text (if model.gameState == Start then "0"
            else String.fromInt (round timer) )
        |> sansserif 
        |> bold
        |> filled white
        |> addOutline (solid 0.7) black
        |> scale 0.45
        |> move (5,-6)
    ] |> group |> move (75,50)
    
test1 = "The quick brown fox jumps over the lazy dog. Have you\nchecked out the movie yet?? The game is finally working\nyay!"
test2 = "Some people combine touch typing and hunt and peck by\nusing a buffering method. In the buffer method, the\ntypist looks at the source. The quick brown fox jumps\nover the lazy dog."
test3 = "You know, I know this steak doesn't exist. I know that when\nI put it in my mouth, the Matrix is telling my brain that it\nis juicy and delicious."
test4 = "The first digit of pi is three point one four one five nine\ntwo six five!"
  
warningBox mistake = 
    [ roundedRect 45 15 2
        |> filled orange
        |> addOutline (solid 1) black
    , text "Wrong letter!"
        |> sansserif  |> bold
        |> filled black
        |> scale 0.4
        |> move (-16,2)
     , text (if mistake == " " then "Space"
             else if mistake == "\n" then "Space"
             else mistake)
        |> sansserif  |> bold
        |> filled black
        |> scale 0.4
        |> move (-3,-4)
    ] |> group |> move (5,11)

-- Includes calculations for how many coins the users earn
calcCoins model =
  let wordCount = toFloat (String.length model.input)
      accuracy =  wordCount / (wordCount + (toFloat model.mistakes))
  in
      round (20 * accuracy)

  
victoryBox model = 
  let coins = calcCoins model
      wordCount = toFloat (String.length model.input)
      accuracy =  round ( wordCount / (wordCount + (toFloat model.mistakes)) * 100)
  in
    [ roundedRect 55 18 2
        |> filled green
        |> addOutline (solid 1) black
        |> move (0,6.5)
    , text "Accuracy : "
        |> sansserif  |> bold |> italic
        |> filled black
        |> scale 0.45
        |> move (-25,8)
    , text (String.fromInt accuracy ++ "%")
        |> sansserif  |> italic
        |> filled black
        |> scale 0.45
        |> move (10,8)
    , text "Reward : "
        |> sansserif  |> bold |> italic
        |> filled black
        |> scale 0.45
        |> move (-22,1)
    , text ((String.fromInt coins) ++ " coins")
        |> sansserif  |> italic
        |> filled black
        |> scale 0.45
        |> move (3,1)
    ] |> group |> move (-45,5)

--------- Calculations ---------

{-checkText : String -> String -> Bool
checkText correct input =
  case (String.toList correct, String.toList input) of
    (x::xs,y::ys) -> (x == y) && (checkText (String.fromList xs) (String.fromList ys))
    _ -> True -}

-- Checks if the start of the input matches the start of the required text
checkText correct input =
  let inputSize = (String.length input)
      letter = String.slice (inputSize - 1) (inputSize) correct
  in 
  if correct == input then 
            Finished input
  else if (String.endsWith letter input) then 
            Correct input
  else if (letter == "\n") && (String.endsWith " " input) then 
            Correct ((String.dropRight 1 input) ++ "\n")
            -- Lets the user types space instead of line break
  else 
            Wrong letter

-- Code for keyboard input
typeAndDelete soFar code =
    if String.length code == 1 then 
        soFar ++ code 
    else if code == "Backspace" then
        String.dropRight 1 soFar
    else if code == "Enter" then
        soFar ++ "\n"
    else soFar

-- Randomly generate an index to choose which passage to display
indexGenerator model = (int 0 ((List.length model.tests) - 1) )

getSeed model = initialSeed (round <| model.time * 1000)

getIndex model = 
  let (index, seed) = Random.step (indexGenerator model) (getSeed model)
  in
  index

-- Get a passage based on index
getPassage passages i =
  case passages of
    (x :: xs) -> if i == 0 then x
                 else getPassage xs (i - 1)
    _ -> ""

--------- Game Model ---------

type Msg = Tick Float
         | WindowResize (Maybe ( Float, Float ))
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float )
         | KeyDown String
         | KeyUp String
         | NoOp
         | TypedKey String
         | MouseOver String
         | MouseOff
         | BackKey
         | CapsToggle
         | StartTimer
         | Exit
         
type State = NotTyping 
           | IsTyping

type GameState = Start
               | Ongoing
               | End

type TextState = Correct String
               | Wrong String
               | Finished String

type alias Model =  { state : State  
                    , window : Window -- do not change (used for resizing window)
                    , time : Float    -- not recommended that you change this
                    , startTime : Float -- The time the users start playing
                    , finalTime : Float -- Final time when user wins
                    , hover : String -- What is the user hovering over wtih
                    , input : String -- What the user typed
                    , output : String -- What the user should type
                    , caps : Bool -- Did the user enable caps for the onscreen keyboard
                    , showMistake : Bool -- Display the mistake textbox
                    , mistake : String -- The key the user needs to type
                    , gameState : GameState -- Game State (Start, Ongoing, End)
                    , mistakes : Int -- How many mistakes in total
                    , tests : List String -- All the possible passages the user can type
                    , exit : Bool
                    , coins : Int
                    }

init =  { state = IsTyping 
        , time = 0
        , window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 }
        , startTime = 0
        , finalTime = 0
        , hover = ""
        , input = ""
        , output = "Click Start!"
        , caps = False
        , showMistake = False
        , mistake = ""
        , gameState = Start
        , mistakes = 0
        , tests = [test1, test2, test3, test4]
        , exit = False
        , coins = 0
        }

update msg model = 
  case msg of
    -- TODO handle new messages here
    TypedKey s -> (if model.gameState == Ongoing then
                  ( (case checkText model.output (model.input ++ s) of
                     Correct i -> { model | input = i, showMistake = False}
                     Wrong m -> { model | showMistake = True, mistake = m, mistakes = model.mistakes + 1 }
                     Finished i -> { model | input = i, showMistake = False
                       , finalTime = model.time - model.startTime, gameState = End
                       , coins = calcCoins model }
                     )
                   , Cmd.none)
                   else (model, Cmd.none)
                   )
    BackKey -> ( { model | input = String.dropRight 1 model.input }, Cmd.none)
    MouseOver s -> ( { model | hover = s }, Cmd.none)
    MouseOff -> ( { model | hover = "" }, Cmd.none)
    CapsToggle -> ( { model | caps = not model.caps }, Cmd.none)
    StartTimer -> (if model.gameState == Start then
                  ( { model | startTime = model.time
                            , output = getPassage (model.tests) (getIndex model)              
                            , gameState = Ongoing }, Cmd.none)
                   else (model, Cmd.none)
                  )
    Exit -> ({ model | exit = True }, Cmd.none)
    
    -- get keyboard input
    KeyUp _ -> (model,Cmd.none)
    KeyDown code -> (if model.gameState == Ongoing then
                      ( case model.state of
                         NotTyping -> (model, Cmd.none)
                         -- TODO handle keyboard input here
                         IsTyping -> ( (case checkText model.output (typeAndDelete model.input code) of
                                       Correct i -> { model | input = i, showMistake = False}
                                       Wrong m -> { model | showMistake = True, mistake = m, mistakes = model.mistakes + 1 }
                                       Finished i -> { model | input = i, showMistake = False, 
                                             finalTime = model.time - model.startTime, gameState = End
                                             , coins = calcCoins model}
                                       )
                                       , Cmd.none)
                       )
                    else (model, Cmd.none) 
                    )

    -- don't change these unless you really need to
    Tick t -> ( { model | time = t }, Cmd.none )
    WindowResize mWH ->
      case mWH of
        Just ( w, h ) ->
          ( { model | window = didResize model.window w h
              }
          , Cmd.none
          )
        -- need to get viewport size after the app starts
        Nothing ->
          ( model
          , getViewportSize
          )
    ReturnPosition message ( x, y ) ->
        let
            ( newModel, userCmds ) =
                update
                    (message (convertCoords model.window ( x, y ) ))
                    model
        in
        ( newModel, userCmds )
    NoOp -> ( model, Cmd.none )
    
-------------------------------------------------------------------------------
box clr clr2 n = 
    if n <= 0 then [] |> group 
    else
        [ square 10
            |> filled clr
            |> addOutline (solid 0.1) clr2
        , box clr clr2 (n - 1)
            |> move (20,0)
        ] |> group
    
bg2 = [ box black black 10
            |> move (-90,55)
      , box (hsl (degrees 244) 0.04 0.145) black 9
            |> move (-80,55)
      , box black black 9
            |> move (-80,45)
      , box (hsl (degrees 244) 0.04 0.145) black 10 
            |> move (-90,45) 
      ] |> group

bg = 
    [ bg2 
    , bg2 |> move (0,-20)
    , bg2 |> move (0,-40)  
    ] |> group
    
typingTest =
    [ rect 60 12
        |> filled black 
        |> addOutline (solid 1.7) white
    , text "TYPING TEST"
        |> sansserif
        |> bold
        |> filled white
        |> addOutline (solid 0.5) (hsl (degrees 244) 0.04 0.145)
        |> scale 0.6
        |> move (-24,-2.5)
    ] |> group |> move (0,51)


typeBox = 
    [ roundedRect 156 45 3
        |> filled white
        |> addOutline (solid 2) black
        |> move (0, 17)
    , box (hsl (degrees 231) 0.02 0.959) (hsl (degrees 231) 0.02 0.959) 7
        |> move (-60,32)
    , box (hsl (degrees 231) 0.02 0.959) (hsl (degrees 231) 0.02 0.959) 8
        |> move (-70,22)
    , box (hsl (degrees 231) 0.02 0.959) (hsl (degrees 231) 0.02 0.959) 7
        |> move (-60,12)
    , box (hsl (degrees 231) 0.02 0.959) (hsl (degrees 231) 0.02 0.959) 7
        |> move (-70,2)
    ] |> group
    
