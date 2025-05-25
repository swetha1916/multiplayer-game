module Main exposing (main)

-- Step 1 Import the Games
import TitlePage
import Painting
import Wordle
import WhackAMole
import TypingTest
import MemoryGame
import Shop
import Dict exposing (Dict)
import Random exposing (..)
import Browser.Events exposing (onKeyDown, onKeyUp)
import Json.Decode as D

myShapes model =
    
    case model.state of
      Title -> [ square 500 
                  |> filled (hsl (degrees 249) 0.792 0.906)
              , snowflakes
              , 
              [ text "The"
                           |> centered
                           |> bold
                           |> filled black
                           |> addOutline (solid 0.2) black
                           |> move (0,28)
                           |> scale 1
               , text "MULTIVERSE"
                           |> centered
                           |> bold
                           |> filled black
                           |> addOutline (dotted 0.2) black
                           |> move (0,5)
                           |> scale 1.7 ] |> group |> move (0,17)
               ,
               [ roundedRect 30 15 2
                                |> filled (hsl (degrees 249) 0.19 0.579)
                                |> addOutline (dotted 1) white
                                |> move (-1,-1)
                              ,
                         text " - Click to - "
                                |> filled white
                                |> scale 0.28
                                |> move (-9,1)
                              ,
                         text "PLAY GAME"
                                |> bold
                                |> filled white
                                |> scale 0.3
                                |> move (-12,-5) ]
                         |> group |> move (2,0) |> scale 2 |> notifyTap Start
              ,
              [ whack |> move (-60,-53)
              , memory |> move (-20,-53)
              , paint |> move (20,-53)
              , typing |> move (60,-53)
              , wordle |> move (100,-53) ] |> group |> move (-5,5)
              ]
      T ->
               [
                rectangle 192.3 130
                  |> filled (radialGradient 
                        [
                          stop (rgb 255 236 131) 10
                        , stop (rgb 250 247 236) 30
                        , stop (rgb 171 220 255) 70
                        , stop (rgb 3 150 255) 90
                        ])
                  |> addOutline (solid 0.5) black 
                , displayCoins model
                , rectangle 192.3 90
                  |> filled (radialGradient 
                        [
                          stop (rgb 65 152 11) 30
                        , stop (rgb 16 123 19) 90 
                        ])
                  |> addOutline (solid 0.5) black
                  |> move (0, -30)
                , [
                  roundedRect 20 10 2
                  |> filled green 
                  |> addOutline (solid 0.5) (if model.hover == "City" then white
                                           else black )
                  |> move (-85,58)
                , text "City" 
                  |> sansserif |> italic 
                  |> filled black
                  |> scale 0.8
                  |> move (-95,55)
                  ] |> group
                    |> notifyTap (GoTo C)
                    |> notifyEnter (MouseHover "City")
                    |> notifyLeave (MouseOff)
                , group
                  [
                    curve (21.394,-7.930) [Pull (-13.91,0.5687) (-22.87,18.628),Pull (-23.97,18.628) (-25.08,18.628),Pull (-35.25,0.1087) (-69.34,-7.930)] 
                      |> filled red
                      |> addOutline (solid 1) (if model.hover == "Shop" then white
                                               else black )
                    , rect 91 5
                      |> filled white
                      |> addOutline (solid 1) black
                      |> move (-23.5,-10.5)
                    , helperfunc1 15 5 30 
                      |> move (-65,-28)
                    , helperfunc2 15 
                      |> move (-65,-13)
                    , rect 90 2
                      |> outlined (solid 1) black
                      |> move (-23,-44.5)
                    , rect 20 0.5 
                      |> outlined (solid 1) black
                      |> move (-24,7.2)
                  ] |> scale 0.5
                    |> move (75,-30)
                    |> notifyTap (GoTo <| Shop (shopInit model) )
                    |> notifyEnter (MouseHover "Shop")
                    |> notifyLeave (MouseOff)
               , (if model.hover == "Shop" then
                   hoverText model "Shop" |> group |> move (63.5,-20)
                  else [] |> group )
               , group
                 [
                    triangle 25
                     |> filled red
                     |> addOutline (solid 0.5) black
                     |> rotate (degrees 90)
                     |> move (0,-25)
                  , helperfunc3 3 20
                    |> rotate (model.time)
                  , helperfunc3 2 30 
                    |> rotate (model.time)
                  , helperfunc4 10
                    |> rotate (model.time)
                 ] |> scale 0.7
                   |> move (60,27)
                   
                , tent "Painting" model 
                    |> move (-40,-20)
                    |> notifyTap (GoTo <| PGame Painting.init)
                , tent "Whack A Mole" model 
                    |> move (-4,20)
                    |> notifyTap (GoTo <| MGame (moleInit model) )
                , tent "Memory Game" model 
                    |> move (25,-20)
                    |> notifyTap (GoTo <| MemGame (memoryInit model) )
                , tent "To be built!" model |> move (50,25)
                
                , [ 
                    roundedRect 20 13 2
                        |> filled green 
                        |> addOutline (solid 0.5) (if model.hover == "Help" then white
                                                 else black )
                        |> move (-85,23)
                    , text "Help"
                        |> italic
                        |> filled black
                        |> scale 0.8
                        |> move (-95,20)
                  ] |> group
                    |> notifyTap (GoTo H)
                    |> notifyEnter (MouseHover "Help")
                    |> notifyLeave (MouseOff)
               
               ]
      C ->
               [
                rectangle 192.3 130
                  |> filled (radialGradient 
                        [
                          stop (rgb 51 0 204) 10
                        , stop (rgb 22 22 120) 70
                        , stop (rgb 2 0 36) 100
                        ])
                  |> addOutline (solid 0.5) black
                , rect 192.3 10
                  |> filled darkGrey
                  |> addOutline (solid 0.5) black
                  |> move (0, -62)
                , displayCoins model
                , stars 4 -- Generate stars with helper function
                  |> move (0, 10)
                , stars 4
                  |> move (-45, 10)
                , stars 3
                  |> move (-80, 15)
                , stars 3
                  |> move (35, 15)
                , [
                  roundedRect 26 10 2
                  |> filled green 
                  |> addOutline (solid 0.5) (if model.hover == "Town" then white
                                           else black )
                  |> move (-82,58)
                , text "Town" 
                  |> sansserif |> italic 
                  |> filled black
                  |> scale 0.8
                  |> move (-95,55)
                  ] |> group
                    |> notifyTap (GoTo T)
                    |> notifyEnter (MouseHover "Town")
                    |> notifyLeave (MouseOff)
                  
                , group
                  [
                  group
                  [
                   curve (-7.377,37.072) [Pull (-7.193,45.925) (-7.008,54.778),Pull (-6.639,45.925) (-6.270,37.072)]
                     |> filled grey
                     |> addOutline (solid 0.5) black
                   , rect 6 2 
                     |> filled grey
                     |> addOutline (solid 0.5) black
                     |> move (-7,36)
                   , rect 10 2.5
                     |> filled grey
                     |> addOutline (solid 0.5) black
                     |> move (-7,33.7)
                   , rect 13 3
                     |> filled grey
                     |> addOutline (solid 0.5) black
                     |> move (-7,31)
                   , rect 20 70
                     |> filled grey
                     |> addOutline (solid 0.5) black
                     |> move (-7,-5.5)
                   , winds 17 3 2 4
                     |> move (0,-38)
                   , winds 17 3 2 4
                     |> move (-4.5,-38)
                   , winds 17 3 2 4
                     |> move (-9,-38)
                   , winds 17 3 2 4
                     |> move (-14,-38)
                  ]  |> move (40,-19)
                  
                , group
                  [
                    curve (-57.54,-8.135) [Pull (-57.54,7.5619) (-57.54,18.259),Pull (-54.96,21.025) (-52.38,23.792),Pull (-52.38,25.636) (-52.38,27.481),Pull (-51.45,27.481) (-50.53,27.481),Pull (-50.53,26.927) (-50.53,26.374),Pull (-49.06,26.374) (-47.58,26.374),Pull (-45.18,28.772) (-42.78,31.170),Pull (-39.83,31.170) (-36.88,31.170),Pull (-36.88,18.259) (-36.88,-8.135), Pull (-36.88,-8.135) (-57.54,-8.135)]
                      |> filled grey
                      |> addOutline (solid 0.5) (if model.hover == "Building2" then white
                                                 else black )
                      |> scale 1.5
                    , rect 1.8 35.6
                      |> filled grey
                      |> addOutline (solid 0.5) (if model.hover == "Building2" then white
                                                 else black )
                      |> scale 1.5
                      |> move (-77.2,14.2)
                    , winds 15 3.5 1.5 2.5
                      |> move (-59,9)
                    , winds 22 3.5 1.5 2.5  
                      |> move (-64.5,-10)
                    , winds 20 3.5 1.5 2.5
                      |> move (-72,-10)
                      
                    , rect 15 20
                      |> filled grey
                      |> addOutline (solid 0.5) (if model.hover == "Building2" then white
                                                 else black )
                      |> move (-54,-2)
                    , winds 8 3.5 1.5 2.5 
                      |> move (-59,-11)
                    , winds 8 3.5 1.5 2.5 
                      |> move (-54,-11)
                    , winds 8 3.5 1.5 2.5 
                      |> move (-49,-11)
                      
                    , rect 8 20
                      |> filled grey
                      |> addOutline (solid 0.5) (if model.hover == "Building2" then white
                                                 else black )
                      |> move (-42.5,-2)
                    , winds 8 3.5 1.5 2.5
                      |> move (-42.5,-11)
                    
                    , rect 16 10
                      |> filled grey
                      |> addOutline (solid 0.5) (if model.hover == "Building2" then white
                                                 else black )
                      |> move (-30.5,-6.8)
                    , winds 3 3.5 1.5 2.5 
                      |> move (-35.5,-9)
                    , winds 3 3.5 1.5 2.5 
                      |> move (-30.5,-9)
                    , winds 3 3.5 1.5 2.5 
                      |> move (-25.5,-9)
                    ,(if model.hover == "Building2" then
                      hoverText model "Typing Arena"
                      |> group |> move (-60, 50)
                    else
                      [] |> group
                    )
                  ] |> move (30,-47.5) 
                    |> notifyTap (GoTo <| TGame (TypingArena.init, Cmd.none))
                    |> notifyEnter (MouseHover "Building2")
                    |> notifyLeave (MouseOff)
                     
              , group
                 [
                   curve (-22.87,-9) [Pull (-22.87,17.890) (-22.87,38.178),Pull (-21.57,38.178) (-20.28,38.178),Pull (-17.52,35.965) (-14.75,33.752),Pull (-14.75,12.910) (-14.75,-9.1)]
                     |> filled grey
                     |> addOutline (solid 0.5) (if model.hover == "Building3" then white
                                                 else black )
                     |> scale 1.5
                     |> move (40,-46)
                   , winds 15 10 2 4
                     |> move (12,-57)     
                   ,(if model.hover == "Building3" then
                      hoverText model "Wordle"
                      |> group |> move (12, 15)
                    else
                      [] |> group
                    )
                 ] |> notifyTap (GoTo <| WGame Wordle.init )
                   |> notifyEnter (MouseHover "Building3")
                   |> notifyLeave (MouseOff)
                 
               , group
                 [ 
                   curve (-62.34,-12.130) [Pull (-62.34,29.0374) (-62.34,41.210),Pull (-64.36,43.792) (-66.39,46.374),Pull (-68.42,46.374) (-70.45,46.374),Pull (-70.45,47.850) (-70.45,49.325),Pull (-73.03,49.325) (-75.61,49.325),Pull (-75.61,30.697) (-75.61,-12.130)]
                     |> filled grey
                     |> addOutline (solid 0.5) black
                     |> move (0,-48)   
                   , line (0,-40) (0,18)
                     |> filled grey
                     |> addOutline (solid 0.5) black
                     |> move (-67,-20)
                   , winds 28 8 0.5 2
                     |> move (-71.5,-58)
                     
                 ]
                 
               , group
                 [
                   curve (42.051,-4.472) [Pull (42.051,30.985) (42.051,41.498),Pull (44.265,43.711) (46.478,45.925),Pull (47.769,45.925) (49.060,45.925),Pull (49.060,41.682) (49.060,37.440),Pull (49.982,36.518) (50.904,35.596),Pull (50.904,30.432) (50.904,25.268),Pull (54.040,25.268) (57.175,25.268),Pull (57.175,10.512) (57.175,-4.242)]
                   |> filled grey
                   |> addOutline (solid 0.5) black
                   |> scale 1.5
                   |> move (-18,-53)
                   , winds 17 3 2 4
                       |> move (48,-58)
                   , winds 17 3 2 4
                       |> move (52.3,-58)
                   , winds 15 3 2 4
                       |> move (56.3,-58)
                   , winds 11 3 2 4
                       |> move (63.3,-58)
                   , line (0,-15) (0,-60)
                       |> outlined (solid 0.5) black
                       |> move (58.3,0)
                 ]
              , group
                [
                  curve (57.175,-4.242) [Pull (57.175,13.832) (57.175,31.907),Pull (58.466,33.198) (59.757,34.489),Pull (61.048,33.198) (62.340,31.907),Pull (62.524,14.017) (62.708,-3.873)]
                    |> filled grey
                    |> addOutline (solid 0.5) black
                    |> scale 2
                    |> move (-42,-52)
                  , line (-1,0) (10,0)
                      |> filled grey
                      |> addOutline (solid 0.5) black
                      |> move (73,11)
                  , winds 17 3 2 4
                       |> move (78,-58)
                       
                  
                  , [ 
                    roundedRect 20 13 2
                        |> filled green 
                        |> addOutline (solid 0.5) (if model.hover == "Help" then white
                                                 else black )
                        |> move (-85,23)
                    , text "Help"
                        |> italic
                        |> filled black
                        |> scale 0.8
                        |> move (-95,20)
                  ] |> group
                    |> notifyTap Help
                    |> notifyEnter (MouseHover "Help")
                    |> notifyLeave (MouseOff)
                ]
                ] |> move (0, -5)
               ]
      PGame pModel ->
            [
              Painting.myShapes pModel
                |> group
                |> GraphicSVG.map PMsg
            ]
      TGame (tModel, x) ->
            [
              TypingArena.myShapes tModel
                |> group
                |> GraphicSVG.map TMsg
            ]
      WGame wModel ->
            [
              Wordle.myShapes wModel
                |> group
                |> GraphicSVG.map WMsg
            ]
      MGame mModel ->
            [
              Mole.myShapes mModel
                |> group
                |> GraphicSVG.map MMsg
            ]
      MemGame memModel ->
            [
              MemoryGame.myShapes memModel
                |> group
                |> GraphicSVG.map MemMsg
            ]
      Shop sModel ->
            [
              Shop.myShapes sModel
                |> group
                |> GraphicSVG.map ShopMsg
            ]
      -- the how to play menu
      H  ->   [
                square 200
                  |> filled (rgb 3 150 255)
                  
                ,
                [ 
                  roundedRect 20 13 2
                      |> filled green 
                      |> addOutline (solid 0.5) (if model.hover == "Back" then white
                                               else black )
                      |> move (-85,23)
                  , text "Back"
                      |> italic
                      |> filled black
                      |> scale 0.8
                      |> move (-95,20)
                ] |> group
                  |> move (0,30)
                  |> notifyTap (GoTo T)
                  |> notifyEnter (MouseHover "Back")
                  |> notifyLeave (MouseOff)
               
               -- all the buttons leading to the sub pages
               ,
                [ 
                  roundedRect 38 13 2
                      |> filled green 
                      |> addOutline (solid 0.5) (if model.hover == "Painting" then white
                                               else black )
                      |> move (-78,23)
                  , text "Painting"
                 
                      |> italic
                      |> filled black
                      |> scale 0.8
                      |> move (-95,20)
                ] |> group
                  |> move (72,20)
                  |> notifyTap (GoTo Painting)
                  |> notifyEnter (MouseHover "Painting")
                  |> notifyLeave (MouseOff)
                  
                ,
                [ 
                  roundedRect 58 13 2
                      |> filled green 
                      |> addOutline (solid 0.5) (if model.hover == "Whack-a-mole" then white
                                               else black )
                      |> move (-66,23)
                  , text "Whack-a-mole"
                 
                      |> italic
                      |> filled black
                      |> scale 0.8
                      |> move (-95,20)
                ] |> group
                  |> move (70,0)
                  |> notifyTap (GoTo Whack)
                  |> notifyEnter (MouseHover "Whack-a-mole")
                  |> notifyLeave (MouseOff)
                  
                
                ,
                [ 
                  roundedRect 60 13 2
                      |> filled green 
                      |> addOutline (solid 0.5) (if model.hover == "Memory Game" then white
                                               else black )
                      |> move (-66,23)
                  , text "Memory Game"
                 
                      |> italic
                      |> filled black
                      |> scale 0.8
                      |> move (-95,20)
                ] |> group
                  |> move (70,-20)
                  |> notifyTap (GoTo Memory)
                  |> notifyEnter (MouseHover "Memory Game")
                  |> notifyLeave (MouseOff)
                
                ,
                [ 
                  roundedRect 30 13 2
                      |> filled green 
                      |> addOutline (solid 0.5) (if model.hover == "Wordle" then white
                                               else black )
                      |> move (-80,23)
                  , text "Wordle"
                 
                      |> italic
                      |> filled black
                      |> scale 0.8
                      |> move (-95,20)
                ] |> group
                  |> move (70,-40)
                  |> notifyTap (GoTo Wordle)
                  |> notifyEnter (MouseHover "Wordle")
                  |> notifyLeave (MouseOff)
                  
                ,
                [ 
                  roundedRect 55 13 2
                      |> filled green 
                      |> addOutline (solid 0.5) (if model.hover == "Typing Arena" then white
                                               else black )
                      |> move (-69,23)
                  , text "Typing Arena"
                 
                      |> italic
                      |> filled black
                      |> scale 0.8
                      |> move (-95,20)
                ] |> group
                  |> move (72,-60)
                  |> notifyTap (GoTo Typing)
                  |> notifyEnter (MouseHover "Typing Arena")
                  |> notifyLeave (MouseOff)
               
                
              ]
      -- display how to play the games
      Painting -> 
                  [ 
      
                    [ 
                      roundedRect 20 13 2
                          |> filled green 
                          |> addOutline (solid 0.5) (if model.hover == "Back" then white
                                                   else black )
                          |> move (-85,23)
                      , text "Back"
                          |> italic
                          |> filled black
                          |> scale 0.8
                          |> move (-95,20)
                    ] |> group
                      |> move (0,30)
                      |> notifyTap (GoTo H)
                      |> notifyEnter (MouseHover "Back")
                      |> notifyLeave (MouseOff)
                      
                      
                    , text "Rules"
                        |> bold
                        |> centered
                        |> italic
                        |> filled black
                        |> move (0,50)
              
                    , message "1. Choose a stencil (outline) with arrow keys" black
                        |> group
                        |> move (0,30)
                      
                    , message "2. Paint over the outline and anywhere else!" black
                        |> group
                        |> move (0,10)
                        
                    , message "3. You can toggle the outline and clear canvas" black
                        |> group
                        |> move (0,-10)
                        
                    , message "4. Hit finish after you finish painting!" black
                        |> group
                        |> move (0,-30)
                    
                    , message "5. Make sure to fill in outline completely first" black
                        |> group
                        |> move (0,-50)
                  ] 
      Wordle -> 
                  [
                     [ 
                      roundedRect 20 13 2
                          |> filled green 
                          |> addOutline (solid 0.5) (if model.hover == "Back" then white
                                                   else black )
                          |> move (-85,23)
                      , text "Back"
                          |> italic
                          |> filled black
                          |> scale 0.8
                          |> move (-95,20)
                    ] |> group
                      |> move (0,30)
                      |> notifyTap (GoTo H)
                      |> notifyEnter (MouseHover "Back")
                      |> notifyLeave (MouseOff)
                      
                      
                    , text "Rules"
                        |> bold
                        |> centered
                        |> italic
                        |> filled black
                        |> move (0,50)
              
                    , message "1. There are 6 attempts to win this game" black
                        |> group
                        |> move (-6,30)
                      
                    , message "2. Enter the letters of your guess" black
                        |> group
                        |> move (-18,10)
                        
                    , message "3. Red squares display the wrong letters" black
                        |> group
                        |> move (-6,-10)
                        
                    , message "4. Green squares display the correct letters" black
                        |> group
                        |> move (-1,-30)
                    
                    , message "5. Hints are given at the 3rd and 5th guesses" black
                        |> group
                        |> move (2,-50)
                  ] 
      Whack -> 
                  [
                    [ 
                      roundedRect 20 13 2
                          |> filled green 
                          |> addOutline (solid 0.5) (if model.hover == "Back" then white
                                                   else black )
                          |> move (-85,23)
                      , text "Back"
                          |> italic
                          |> filled black
                          |> scale 0.8
                          |> move (-95,20)
                    ] |> group
                      |> move (0,30)
                      |> notifyTap (GoTo H)
                      |> notifyEnter (MouseHover "Back")
                      |> notifyLeave (MouseOff)
                    
                    , text "Rules"
                        |> bold
                        |> centered
                        |> italic
                        |> filled black
                        |> move (0,50)
              
                    , message "1. Moles will pop up every 2-3 seconds in waves" black
                        |> group
                        |> move (0,30)
                      
                    , message "2. Click on the moles when they pop up!" black
                        |> group
                        |> move (0,10)
                        
                    , message "3. Hitting a mole will award 2 points" black
                        |> group
                        |> move (0,-10)
                        
                    , message "4. Hitting a mole that hiding will subtract a point" black
                        |> group
                        |> move (0,-30)
                    
                    , message "5. 30 seconds to hit all the moles!" black
                        |> group
                        |> move (0,-50)
                  ] 
      Memory -> 
                  [
                    [ 
                      roundedRect 20 13 2
                          |> filled green 
                          |> addOutline (solid 0.5) (if model.hover == "Back" then white
                                                   else black )
                          |> move (-85,23)
                      , text "Back"
                          |> italic
                          |> filled black
                          |> scale 0.8
                          |> move (-95,20)
                    ] |> group
                      |> move (0,30)
                      |> notifyTap (GoTo H)
                      |> notifyEnter (MouseHover "Back")
                      |> notifyLeave (MouseOff)
                      
                      
                    , text "Rules"
                        |> bold
                        |> centered
                        |> italic
                        |> filled black
                        |> move (0,50)
                      
                    , message "1. There is no time limit to this game" black
                        |> group
                        |> move (-6,20)
                      
                    , message "2. Flip the cards by clicking on it" black
                        |> group
                        |> move (-13,0)
                        
                    , message "3. Match cards having the same shapes" black
                        |> group
                        |> move (0,-20)
                        
                  ] 
      Typing -> 
                  [
                   
                    [ 
                      roundedRect 20 13 2
                          |> filled green 
                          |> addOutline (solid 0.5) (if model.hover == "Back" then white
                                                   else black )
                          |> move (-85,23)
                      , text "Back"
                          |> italic
                          |> filled black
                          |> scale 0.8
                          |> move (-95,20)
                    ] |> group
                      |> move (0,30)
                      |> notifyTap (GoTo H)
                      |> notifyEnter (MouseHover "Back")
                      |> notifyLeave (MouseOff)
                      
                    , text "Rules"
                        |> bold
                        |> centered
                        |> italic
                        |> filled black
                        |> move (0,50)
              
                    , message "1. Hit the start button to start the timer!" black
                        |> group
                        |> move (0,30)
                      
                    , message "2. Type the words displayed in the box" black
                        |> group
                        |> move (0,10)
                        
                    , message "3. There will be a pop up warning you of typos" black
                        |> group
                        |> move (0,-10)
                        
                    , message "4. Try to type as fast as possible!" black
                        |> group
                        |> move (0,-30)
                    
                    , message "5. Coins will be awarded based on accuracy" black
                        |> group
                        |> move (0,-50)
                  ] 

background = 
            [
             rectangle 192.3 130
                  |> filled (radialGradient 
                        [
                          stop (rgb 51 0 204) 10
                        , stop (rgb 22 22 120) 70
                        , stop (rgb 2 0 36) 100
                        ])
                  |> addOutline (solid 0.5) black
             ]
             
message txt col = 
            [
              text txt 
                |> sansserif
                |> centered
                |> filled col    
                |> scale 0.6
            ]


displayCoins model = [
                
                  roundedRect 50 10 2
                  |> filled green 
                  |> addOutline (solid 0.5) black
                  |> move (0, 3)
                , text ("Coins: " ++ (String.fromInt model.coins) )
                  |> sansserif |> centered 
                  |> filled black
                  |> scale 0.8
               ] |> group |> move (70,55)

helperfunc1 x l b = 
               if x > 0
               then [
                     rect l b 
                     |> filled (if modBy 2 x == 0 then red
                                else white )
                     |> addOutline (solid 1) black
                     , helperfunc1 (x-1) l b |> move (6,0)
                    ] |> group
               else
               [] |> group
               
helperfunc2 x = 
                if x > 0
                then [
                      wedge 3 0.5
                      |> outlined (solid 1) black
                      |> rotate (degrees -90)
                      , helperfunc2 (x-1) |> move (6,0)
                     ] |> group
                else [] |> group
                
helperfunc3 x sz = 
                if x > 0
                then group 
                     [
                      circle sz
                      |> outlined (solid 0.5) black
                      , helperfunc3 (x-1) sz |> scale 0.9
                     ] 
                else group []
                
helperfunc4 x = 
                if x > 0
                then group
                     [
                      rect 59 1 
                      |> outlined (solid 0.3) black
                      , helperfunc4 (x-1) |> rotate (degrees 20)
                     ]
                else 
                group []
                
winds x a b pos = 
          if x > 0
          then group 
               [
                 rect a b 
                 |> filled yellow 
                 |> addOutline (solid 0.1) black
                 , winds (x-1) a b pos |> move (0,pos)
               ]
         else group []
                
cabin = 
        [
         wedge 3.5 0.5
         |> outlined (solid 0.5) black
         |> rotate (degrees 90)
         |> move (0,10)
         , wedge 5 0.5 
         |> outlined (solid 0.5) black
         |> rotate (degrees -90)
         |> move (0,2)
         , rect 8.5 6 
         |> outlined (solid 0.5) black
         |> move (0,5)
         , rect 10 2 
         |> outlined (solid 0.5) black
         |> move (0,9)
        ]     
        
tent x model = 
        let name = "Tent" ++ x
        in [
                  [
                    helperfunc1 15 5 30
                    , polygon [(0,0),(0,-10),(30,0)]
                        |> filled white
                        |> addOutline (solid 0.5) black
                        |> move (13,-26)
                        |> scale 0.8
                        |> rotate (degrees 40)
                    , triangle 16.5
                        |> filled (if model.hover == name then (hsl 0 0 0.4)
                                   else black)
                        |> move (-9,-40)
                        |> rotate (degrees 88)
                    , polygon [(0,0),(0,-10),(30,0)]
                        |> filled white
                        |> addOutline (solid 0.5) black
                        |> move (-48,-47)
                        |> scale 0.8
                        |> rotate (degrees 120)
                    , helperfunc2 15
                      |> move (0,15)
                    , curve (-79.14,-7.193) [Pull (-59.07,1.1816) (-29.87,38.916),Pull (-9.233,8.5016) (17.172,-7.193)]
                      |> filled red
                      |> addOutline (solid 1) (if model.hover == name then white
                                               else black )
                      |> move (73,22)
                  ]
                    |> group
                    |> scale 0.4
                    |> move (-50,-30)
                    |> notifyEnter (MouseHover name)
                    |> notifyLeave (MouseOff)                 
                 ,
                 (if model.hover == name then                     
                         hoverText model x
                           |> group
                           |> notifyEnter (MouseHover name)
                           |> notifyLeave (MouseOff)
                           |> move (-32.5,-5)                         
                       else [] |> group)
                 ] |> group

hoverText model x =  [ roundedRect 45 8 2
              |> filled green 
              |> addOutline (solid 0.5) black
              |> move (0, 3)
            , text x 
              |> sansserif |> centered |> size 6
              |> filled black
            ]

-- Generate a random star cluster group
stars n = if n > 0 then
               (group [
                  circle 1
                    |> filled yellow                  
                , stars (n - 1)
                    |> move (
                         5 + 1.9 * (toFloat n)
                      ,  10 + 1.44 * (toFloat n)
                    )
                , stars (n - 1)
                    |> move (
                         20 - 3.1415 * (toFloat n)
                      ,  5 - 4.2 * (toFloat n)
                    )
                ])
           else
             group []

type Msg = Tick Float GetKeyState 
         | KeyUp String
         | KeyDown String
         | MouseHover String -- Hover Effects
         | MouseOff
         | Start
         ---------------------
         | PMsg Painting.Msg -- Step 2 Game messages
         | TMsg TypingArena.Msg
         | WMsg Wordle.Msg
         | MMsg Mole.Msg
         | MemMsg MemoryGame.Msg
         | ShopMsg Shop.Msg
         | GoTo State -- Go to each area
         | Help
         
         

type State = Title
           | T -- Town menu
           | C -- City menu
           | H -- How to Play menu
           | Painting
           | Whack
           | Memory
           | Wordle
           | Typing
           | MemGame MemoryModel
           | PGame PaintingModel -- Step 3 define states
           | TGame TypingModel
           | WGame WordleModel
           | MGame MoleModel
           | Shop ShopModel
           
           
type alias Model = { time : Float 
                   , state : State
                   , hover : String 
                   , coins : Int 
                   , inventory : List (String)
                   , hammer : Color
                   }

update msg model = ((case msg of
                     Tick t key -> { model | time = t, state = case model.state of -- Step 4 send tick updates to games
                                                       PGame submodel ->
                                                               Painting.update (Painting.Tick t key) submodel
                                                                 |> PGame
                                                       TGame (submodel, x) ->
                                                               let newState = TypingArena.update (TypingArena.Tick t) submodel
                                                               in
                                                               newState |> TGame
                                                       WGame submodel ->
                                                               Wordle.update (Wordle.Tick t key) submodel
                                                                 |> WGame
                                                       MGame submodel ->
                                                               Mole.update (Mole.Tick t key) submodel
                                                                 |> MGame
                                                       MemGame submodel ->
                                                               MemoryGame.update (MemoryGame.Tick t key) submodel
                                                                 |> MemGame
                                                       Shop submodel ->
                                                               Shop.update (Shop.Tick t key) submodel
                                                                 |> Shop                
                                                       otherwise -> otherwise
                                 }
                     MouseHover s -> { model | hover = s }
                     MouseOff -> { model | hover = "" }
                     Start -> { model | state = T }
                     GoTo state -> { model | state = state }
                     Help -> {model | state = if (model.state == T || model.state == C) then H else T}
                     PMsg submsg -> case model.state of -- Step 5 Constantly update the games
                                       PGame submodel ->
                                         let
                                           newState = Painting.update submsg submodel
                                         in
                                           { model | state = if newState.exit
                                                             then T
                                                             else PGame newState
                                                   , coins = if newState.exit && newState.gameState == Painting.End
                                                             then model.coins + 10
                                                             else model.coins
                                           }
                                       otherwise -> model
                     TMsg submsg -> case model.state of 
                                       TGame (submodel, x) ->
                                         let
                                           (newState, y) = TypingArena.update submsg submodel
                                         in
                                           { model | state = if newState.exit
                                                             then C
                                                             else TGame (newState, y)
                                                   , coins = if newState.exit && newState.gameState == TypingArena.End
                                                             then model.coins + newState.coins
                                                             else model.coins
                                           }
                                       otherwise -> model
                     WMsg submsg -> case model.state of -- Step 5 Constantly update the games
                                       WGame submodel ->
                                         let
                                           newState = Wordle.update submsg submodel
                                         in
                                           { model | state = if newState.exit
                                                             then C
                                                             else WGame newState
                                                   , coins = if newState.exit
                                                             then model.coins + newState.coins
                                                             else model.coins
                                           }
                                       otherwise -> model
                     MMsg submsg -> case model.state of -- Step 5 Constantly update the games
                                       MGame submodel ->
                                         let
                                           newState = Mole.update submsg submodel
                                         in
                                           { model | state = if newState.exit
                                                             then T
                                                             else MGame newState
                                                   , coins = if newState.exit && newState.gamestate == Mole.End
                                                             then model.coins + newState.coins
                                                             else model.coins
                                           }
                                       otherwise -> model
                     MemMsg submsg -> case model.state of -- Step 5 Constantly update the games
                                       MemGame submodel ->
                                         let
                                           newState = MemoryGame.update submsg submodel
                                         in
                                           { model | state = if newState.exit
                                                             then T
                                                             else MemGame newState
                                                   , coins = if newState.exit
                                                             then model.coins + newState.coins
                                                             else model.coins
                                           }
                                       otherwise -> model
                     ShopMsg submsg -> case model.state of 
                                       Shop submodel ->
                                         let
                                           newState = Shop.update submsg submodel
                                         in
                                           { model | state = if newState.exit
                                                             then T
                                                             else Shop newState
                                                   , coins = if newState.exit
                                                             then newState.coins
                                                             else model.coins
                                                   , inventory = if newState.exit
                                                                 then newState.inventory
                                                                 else model.inventory
                                                   , hammer = if (newState.exit)
                                                              then newState.hammerC
                                                              else model.hammer
                                           }
                                       otherwise -> model
                                         
                     KeyUp s -> case model.state of 
                                       TGame (submodel, x) ->
                                         let
                                           (newState, y) = TypingArena.update (TypingArena.KeyUp s) submodel
                                         in
                                           { model | state = TGame (newState, y)
                                           }
                                       otherwise -> model
                     KeyDown s -> case model.state of 
                                       TGame (submodel, x) ->
                                         let
                                           (newState, y) = TypingArena.update (TypingArena.KeyDown s) submodel
                                         in
                                           { model | state = TGame (newState, y)
                                           }
                                       otherwise -> model
                      ), Cmd.none)

init = { time = 0 
       , state = Title
       , hover = ""
       , coins = 0
       , inventory = [ "Default", "Blue Ham" ]
       , hammer = blue
       }

-- Extra step define the game models

type alias PaintingModel = { time : Float 
                   , colour : Color -- Current selected colour
                   , hover : Color -- Which colour button are you hovering over with?
                   , buttonHover : String -- Which button are you hovering over with?
                   , drawing : Bool -- Is the user holding down their mouse
                   , tiles : Dict (Int, Int) Color -- Dictionary of all tiles and the colour painted
                   , outline : Bool -- The outline of what user needs to draw
                   , outlines : List (List (Int, Int) ) -- A list of all the outlines
                   , outlineIndex : Int -- Which outline are they current on
                   , gameState : Painting.GameState -- Start, Ongoing, End
                   , exit : Bool -- Exit the game or not
                   } -- Game States

shopInit model = { time = 0 
       , hover = ""
       , area = 0
       , exit = False
       , coins = model.coins
       , inventory = model.inventory
       , hammerC = model.hammer
       }

type alias ShopModel = { time : Float 
                   , hover : String -- Which button is the user hovering over
                   , area : Int -- Which area is the user in by index
                   , exit : Bool
                   , coins : Int
                   , inventory : List (String)
                   , hammerC : Color
                   }

type alias TypingModel = ({ caps : Bool
      , exit : Bool
      , finalTime : Float
      , gameState : TypingArena.GameState
      , hover : String.String
      , input : String.String
      , mistake : String.String
      , mistakes : Int
      , output : String.String
      , showMistake : Bool
      , startTime : Float
      , state : TypingArena.State
      , tests : List String.String
      , time : Float
      , coins : Int
      , window : { ch : Float, cw : Float, sh : Float, sw : Float }
      }, Cmd TypingArena.Msg)
      
type alias WordleModel = { time : Float , mode: Wordle.Mode , col : String, attempts: Int, string: String
                    , checkstrings: List String, words: List String, val: Bool, over : Bool, exit : Bool, coins : Int}
                
moleInit model = { time = model.time
       , score = 0 
       , gamestate = Mole.Start
       , pt = (0,0)
       , mouse = Nothing
       , moles = (List.repeat 9 Mole.MoleDown)
       , seed = initialSeed 0
       , intervalTime = model.time
       , coins = 0
       , startTime = model.time
       , exit = False
       , col = model.hammer}                
                
type alias MoleModel = { coins : Int
    , exit : Bool
    , gamestate : Mole.GameState
    , intervalTime : Float
    , moles : List Mole.MoleState
    , mouse : Maybe Int
    , pt : ( Float, Float )
    , score : Int
    , seed : Seed
    , startTime : Float
    , time : Float
    , col : Color
    }

type alias MemoryModel = { cards : List ( Int, Int )
    , check : Bool
    , coins : Int
    , currentstate : MemoryGame.CurrentState
    , exit : Bool
    , score : Int
    , startTime : Float
    , state : MemoryGame.State
    , time : Float
    }

memoryInit model = 
  let 
    --(newCards, newSeed) = shuffleCards [1,1,2,2,3,3,4,4] (Random.initialSeed 7)
    --newCards = [1,1,2,2,3,3,4,4]
      newCards = getPassage [cards1,cards2,cards3,cards4] (getIndex (Random.initialSeed 7))
  in
    { time = 0
    , cards = List.indexedMap ( \ idx c -> (idx,c) ) newCards
    , state = MemoryGame.Waiting
    , currentstate = MemoryGame.Game
    , check = False
    , score = 0
    , exit = False
    , coins = 0
    , startTime = model.time
    }

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

cards1 = [3,1,2,1,4,3,2,4]
cards2 = [1,3,2,1,4,3,2,4]
cards3 = [4,1,3,2,1,4,3,2]
cards4 = [1,4,2,1,3,4,2,3]

-- Your subscriptions go here
subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch 
  [onKeyUp (D.map KeyUp (D.field "key" D.string))
  , onKeyDown (D.map KeyDown (D.field "key" D.string))
  ]

-- Your main function goes here
main : EllieAppWithTick () Model Msg
main = 
  ellieAppWithTick Tick 
    { init = \flags -> (init, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- You view function goes here
view : Model -> { title: String, body : Collage Msg }
view model = 
  {
    title = "Multiverse"
  , body = collage 192 128 (myShapes model)
  }

------------------------------------------------------------------------------------
paint = 
    [ square 18
        |> filled black
        |> addOutline (solid 1.5) white
        |> move (-10,2)
    , paintBrush |> scale 0.9 |> rotate (degrees -10) |> move (-14,6) ] |> group |> move (-5,0)

whack = 
    [ square 18
        |> filled black
        |> addOutline (solid 1.5) white
        |> move (-10,2)
    , mole True |> scale 0.35 |> move (-16,10) ] |> group |> move (-5,0)
    
typing = 
    [ square 18
        |> filled black
        |> addOutline (solid 1.5) white
        |> move (-10,2)
    , flag |> scale 0.5 |> move (-12,-5.5) 
    , square 10
        |> filled (hsl (degrees 249) 0.792 0.906)
        |> move (-10,-12.5) ] |> group |> move (-5,0)
    
wordle = 
    [ square 18
        |> filled black
        |> addOutline (solid 1.5) white
        |> move (-10,2)
    , box (rgb 120 124 127) (0,0) white |> scale 0.6 |> move (-14,6)
    , box (rgb 200 182 83) (0,0) white |> scale 0.6 |> move (-6,6)
    , box (rgb 108 169 101) (0,0) white |> scale 0.6 |> move (-14,-2)
    , box (rgb 120 124 127) (0,0) white |> scale 0.6 |> move (-6,-2) ] |> group |> move (-5,0)
    
memory = 
    [ square 18
        |> filled black
        |> addOutline (solid 1.5) white
        |> move (-10,2)
    , card |> scale 0.6 |> rotate (degrees 10) |> move (-11,2)
    , card |> scale 0.6 |> rotate (degrees 5) |> move (-9,2) ] |> group |> move (-5,0)

box clr pt clr2 = 
    [ rect 10 10
        |> filled clr
        |> addOutline (solid 1.2) clr2
        |> move pt 
    ]
    |> group
    
mole outline = 
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
    ] |> group
    
    
flag = 
    [ curve (7.1943,55.306) [Pull (2.3981,36.721) (-2.398,18.135),Pull (7.4941,21.705) (17.386,19.634),Pull (27.249,15.868) (35.672,9.1428),Pull (44.785,6.4626) (51.859,9.7423),Pull (56.355,25.030) (60.852,40.318),Pull (54.007,38.298) (46.763,39.718),Pull (38.629,42.215) (31.775,48.711),Pull (27.278,52.650) (22.782,53.508),Pull (16.028,55.407) (7.1943,55.306)]
        |> filled black
        |> addOutline (solid 2) white
    , curve (7.4941,55.606) [Pull (-4.646,11.840) (-16.78,-31.92),Pull (-17.83,-32.49) (-18.88,-31.02),Pull (-6.594,12.889) (5.6955,56.805),Pull (6.8449,57.026) (7.4941,55.606)]
        |> filled (hsl (degrees 46) 0.827 0.912)
        |> addOutline (solid 0.5) black
    , curve (14.688,54.407) [Pull (13.489,50.960) (12.290,47.512),Pull (16.917,47.483) (20.384,46.014),Pull (21.283,49.611) (22.182,53.208),Pull (18.435,54.747) (14.688,54.407)]
        |> filled white
    , curve (12.290,46.913) [Pull (11.411,44.215) (10.491,41.517),Pull (7.3442,41.877) (4.1967,41.517),Pull (4.6562,44.515) (5.3957,47.512),Pull (8.8430,47.742) (12.290,46.913)]
        |> filled white
    , curve (1.4988,33.423) [Pull (4.9461,34.073) (8.3934,33.723),Pull (7.4840,30.126) (6.8946,26.529),Pull (2.9976,26.139) (-0.899,25.030),Pull (0.2997,29.227) (1.4988,33.423)]
        |> filled white
    , curve (10.791,41.517) [Pull (9.7423,37.920) (8.6932,34.323),Pull (12.740,34.083) (16.786,32.524),Pull (17.676,36.271) (18.885,40.018),Pull (14.838,41.198) (10.791,41.517)]
        |> filled white
    , curve (7.1943,26.229) [Pull (5.9251,23.231) (5.0960,20.234),Pull (9.1428,20.344) (13.189,19.934),Pull (13.739,22.932) (14.688,25.929),Pull (10.941,26.379) (7.1943,26.229)]
        |> filled white
    , curve (20.983,46.313) [Pull (19.714,43.016) (18.885,39.718),Pull (21.782,40.040) (26.079,36.721),Pull (26.758,39.718) (27.878,42.716),Pull (24.581,45.495) (20.983,46.313)]
        |> filled white
    , curve (16.786,32.824) [Pull (15.887,29.526) (14.988,26.229),Pull (19.095,25.750) (22.482,23.831),Pull (23.061,26.829) (24.281,29.826),Pull (20.684,32.245) (16.786,32.824)]
        |> filled white
    , curve (29.976,49.311) [Pull (29.297,46.014) (28.177,42.716),Pull (31.475,40.668) (34.772,38.220),Pull (36.142,41.067) (36.871,43.915),Pull (33.423,46.973) (29.976,49.311)]
        |> filled white
    , curve (26.379,37.021) [Pull (25.550,33.573) (24.281,30.126),Pull (27.728,28.468) (31.175,25.929),Pull (32.344,29.077) (33.274,32.224),Pull (29.826,34.162) (26.379,37.021)]
        |> filled white
    , curve (22.482,23.831) [Pull (22.073,20.983) (20.983,18.135),Pull (24.430,16.447) (27.878,14.238),Pull (29.107,17.236) (29.976,20.234),Pull (26.229,22.232) (22.482,23.831)]
        |> filled white
    , curve (35.072,38.220) [Pull (34.433,35.372) (33.274,32.524),Pull (37.021,30.565) (40.768,29.526),Pull (41.507,32.224) (42.566,34.922),Pull (38.669,35.431) (35.072,38.220)]
        |> filled white
    , curve (31.475,25.629) [Pull (30.896,22.932) (29.676,20.234),Pull (33.423,18.245) (37.170,16.936),Pull (38.220,19.484) (38.669,22.032),Pull (35.072,23.091) (31.475,25.629)]
        |> filled white
    , curve (43.765,40.318) [Pull (43.316,37.620) (42.266,34.922),Pull (46.163,33.183) (50.060,33.723),Pull (51.250,36.271) (51.559,38.819),Pull (47.512,38.869) (43.765,40.318)]
        |> filled white
    , curve (40.768,29.526) [Pull (39.958,26.079) (38.669,22.632),Pull (42.566,21.033) (46.463,20.833),Pull (47.792,24.580) (48.562,28.327),Pull (44.665,27.847) (40.768,29.526)]
        |> filled white
    , curve (37.170,16.037) [Pull (36.571,13.189) (35.372,10.341),Pull (39.119,8.7226) (42.866,8.5433),Pull (44.095,11.690) (44.964,14.838),Pull (40.918,15.257) (37.170,16.037)]
        |> filled white
    , curve (50.060,33.723) [Pull (49.781,31.025) (48.861,28.327),Pull (53.058,28.367) (57.255,28.927),Pull (58.104,31.475) (58.754,34.023),Pull (54.407,33.133) (50.060,33.723)]
        |> filled white
    , curve (46.463,20.833) [Pull (46.014,18.135) (44.964,15.437),Pull (49.161,14.987) (53.358,15.737),Pull (54.407,18.435) (54.857,21.133),Pull (50.510,20.053) (46.463,20.833)]
        |> filled white
    ] |> group |> scale 0.5 |> rotate (degrees 30)
    

card = [ roundedRect 18 25 2
           |> filled blue
           |> addOutline (solid 1) (hsl (degrees 188) 0.874 0.748)
       , snowflake (hsl (degrees 188) 0.874 0.748) white |> scale 0.12 |> move (4,0)
       ] |> group
       
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
--------------------------------------------------------------------------------
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
    
snowflakes = [ snowflake (hsl (degrees 235) 0.032 0.17) white
               |> makeTransparent 0.5
               |> scale 1
               |> move (-35,35)
            , snowflake (hsl (degrees 235) 0.032 0.17) white
               |> makeTransparent 0.4
               |> scale 1
               |> move (90,-40)
            , snowflake (hsl (degrees 235) 0.032 0.17) white
               |> makeTransparent 0.3
               |> scale 0.5
               |> move (60,50)
            , snowflake (hsl (degrees 235) 0.032 0.17) white
               |> makeTransparent 0.4
               |> scale 0.5
               |> move (-30,-70) 
            , snowflake black white
               |> makeTransparent 0.5
               |> scale 0.6
               |> move (-80,-30)
            , snowflake black white
               |> makeTransparent 0.5
               |> scale 0.6
               |> move (120,30)
         ] |> group