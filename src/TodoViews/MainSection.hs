{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TodoViews.MainSection where

import           Control.Monad                                     (forM_,
                                                                    unless,
                                                                    when)
import           Prelude                                           (Maybe (..),
                                                                    all, filter,
                                                                    id, length,
                                                                    not, snd,
                                                                    ($), (++),
                                                                    (-), (.),
                                                                    (==))

import           React.Flux.Rn.Views

import           React.Flux                                        (elemShow,
                                                                    elemString)
import           React.Flux.Rn.Components.Text
import           React.Flux.Rn.Components.TouchableOpacity
import           React.Flux.Rn.Components.TouchableWithoutFeedback
import           React.Flux.Rn.Components.View
import           React.Flux.Rn.Styles.Text
import qualified React.Flux.Rn.Styles.TextInput                    as TI_
import qualified React.Flux.Rn.Styles.View                         as V_

import           Components
import           Dispatcher
import           Store

mainSection todoState@(TodoState todos filt) =
    let doFilter AcceptAll       = id
        doFilter AcceptActive    = filter (not . todoComplete . snd)
        doFilter AcceptCompleted = filter (todoComplete . snd)
        allCompleted = all (todoComplete . snd) todos
    in
        view [ style [ V_.backgroundColor "#fff"
                       , V_.marginHorizontal 30
                       , V_.shadowOffset $ ContentSize 0 2
                       , V_.shadowRadius 4
                       , V_.shadowColor $ Rgba 0 0 0 0.2
                       , V_.shadowOpacity 1
                       ]] $ do
            view [ style [ V_.flexDirection Row ]] $ do
                touchableOpacity [ onPress $ dispatchTodo ToggleAllComplete ] $
                    view [ style [ V_.alignItems Center______
                                   , V_.alignSelf Center____
                                   , V_.flexDirection Column
                                   , V_.width 50
                                   ]] $
                        text [ style [ transform [RotateZ (Deg 90.0)]
                                       , color $ if allCompleted then "#4d4d4d" else "#d9d9d9"
                                       , fontSize 20
                                       , fontFamily "HelveticaNeue"
                                       ]]
                            ">"
                todoTextInput [ TI_.fontStyle Italic
                              , TI_.fontSize 16
                              ] TextInputArgs { tiaPlaceholder = "What needs to be done?"
                                              , tiaSaveAction = SACreate
                                              , tiaOnCancel = []
                                              , tiaValue = Nothing
                                              }

            view [ style [ V_.borderTopWidth 1
                           , V_.borderTopColor "#e6e6e6"
                           ]] $
                forM_ (doFilter filt todos) todoItem

            mainSectionFooter todoState

todoItem = mkView "todo item" $ \(todoIdx, todo) ->
    let isComplete = todoComplete todo
    in
        view [ style [ V_.flexDirection Row ] ] $ do
            unless (todoIsEditing todo) $ do
                touchableWithoutFeedback [ onPress $ dispatchTodo $ TodoSetComplete todoIdx (not isComplete) ] $
                    -- I guess IOS does not support rendering inline SVG, so let's use border and unicode instead of a check-mark image.
                    view [ style [ V_.width 30
                                   , V_.height 30
                                   , V_.alignSelf Center____
                                   , V_.marginLeft 8
                                   , V_.paddingTop 4
                                   , V_.borderWidth 1
                                   , V_.borderRadius 30
                                   , V_.borderColor "#bddad5"
                                   , V_.alignItems Center______
                                   ]] $
                        text [ style [ fontSize 20, color "#5dc2af", fontFamily "HelveticaNeue" ]] $
                            if isComplete then "\x2713" else ""
                touchableOpacity [ onLongPress $ dispatchTodo $ TodoEdit todoIdx ] $
                    view [ style [ V_.padding 15
                                   , V_.flex 1
                                   ]] $
                        text [ style [ fontSize 22
                                       , marginVertical 3
                                       , fontWeight W300
                                       , color $ Color $ if isComplete then "#d9d9d9" else "#4d4d4d"
                                       , fontFamily "HelveticaNeue"
                                       , textDecorationLine $ if isComplete then LineThrough else None__________
                                       ]] $
                            elemString $ todoText todo

            when (todoIsEditing todo) $ do
                touchableWithoutFeedback [ onPress $ dispatchTodo $ TodoDelete todoIdx ] $
                    view [ style [ V_.width 30
                                   , V_.height 30
                                   , V_.alignSelf Center____
                                   , V_.marginLeft 8
                                   , V_.paddingTop 4
                                   , V_.alignItems Center______
                                   ]] $
                        text [ style [ fontSize 20
                                       , fontFamily "HelveticaNeue"
                                       , color "#cc9a9a"
                                       ]]
                            "x"

                view [ style [ V_.flex 1
                               , V_.marginLeft 15
                               , V_.marginTop 1
                               ]] $
                    todoTextInput [] TextInputArgs { tiaPlaceholder = ""
                                                   , tiaSaveAction = SAUpdate todoIdx
                                                   , tiaOnCancel = [CancelUpdateWithDelay todoIdx]
                                                   , tiaValue = Just $ todoText todo
                                                   }

footerStyles = [ color "#777"
               , fontFamily "HelveticaNeue"
               , fontWeight W300
               ]

filterStyle = [ V_.paddingHorizontal 7
              , V_.marginHorizontal 10
              ]
activeFilterStyle = [ V_.borderWidth 1
                    , V_.borderColor $ Rgba 175 47 47 0.2
                    , V_.borderRadius 3
                    ]

mainSectionFooter :: TodoState -> ReactElementM eventHandler ()
mainSectionFooter = mkView "msfooter" $ \(TodoState todos filtering) ->
    let completed = length (filter (todoComplete . snd) todos)
        itemsLeft = length todos - completed
        styling f = style $ filterStyle ++ (if f == filtering then activeFilterStyle else [])
     in
        view [ style [ V_.flexDirection Row
                       , V_.borderTopWidth 1
                       , V_.borderTopColor "#e6e6e6"
                       , V_.paddingVertical 10
                       , V_.paddingHorizontal 15
                       , V_.justifyContent SpaceBetween_
                       ]] $ do
            view [ style [ V_.flexDirection Row ] ] $ do
                text [ style  $ fontWeight Bold : footerStyles ] $
                    elemShow itemsLeft
                text [ style  footerStyles ] $
                    if itemsLeft == 1 then " item left" else " items left"

            view [ style [ V_.flexDirection Row
                           , V_.justifyContent Center_____
                           , V_.flexWrap Wrap
                           , V_.flex 1
                           ]] $ do
                touchableOpacity [ onPress $ dispatchTodo (SetFilter AcceptAll) ] $
                    view [styling AcceptAll] $
                        text [ style  [fontFamily "HelveticaNeue"] ] "All"
                touchableOpacity [ onPress $ dispatchTodo (SetFilter AcceptActive) ] $
                    view [styling AcceptActive] $
                        text [ style  [fontFamily "HelveticaNeue"] ] "Active"
                touchableOpacity [ onPress $ dispatchTodo (SetFilter AcceptCompleted) ] $
                    view [styling AcceptCompleted] $
                        text [ style  [fontFamily "HelveticaNeue"] ] "Completed"

            touchableOpacity [ onPress $ dispatchTodo ClearCompletedTodos ] $
                view [ style (if completed == 0 then [V_.opacity 0] else []) ] $
                    text [ style (flexWrap Wrap : footerStyles) ]
                        "Clear completed"
