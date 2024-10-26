{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.TodoMVC where

import Prelude hiding (mapM, mapM_, sequence)

import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.Fix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import GHCJS.DOM.Types (JSM)

import Reflex
import Reflex.Dom.Core

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Task
   = Task { taskDescription :: Text
          , taskCompleted :: Bool
          }
   deriving (Show, Eq)

-- | Add a new value to a map; automatically choose an unused key
insertNew_ :: (Enum k, Ord k) => v -> Map k v -> Map k v
insertNew_ v m = case Map.maxViewWithKey m of
  Nothing -> Map.singleton (toEnum 0) v
  Just ((k, _), _) -> Map.insert (succ k) v m

initialTasks :: Map Int Task
initialTasks = Map.empty

--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

-- | Subsets of the task list that can be selected by the user
data Filter
   = All -- ^ All tasks
   | Active -- ^ Uncompleted tasks
   | Completed -- ^ Completed tasks
   deriving (Show, Eq)

-- | Determine whether this Task should be shown when this Filter is in effect
satisfiesFilter :: Filter -> Task -> Bool
satisfiesFilter f = case f of
  All -> const True
  Active -> not . taskCompleted
  Completed -> taskCompleted

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

main :: JSM ()
main = mainWidgetWithCss styleCss todoMVC
  where
    -- WASM backend is currently lacking TH
    styleCss = T.encodeUtf8 "html,\nbody {\n\tmargin: 0;\n\tpadding: 0;\n}\n\nbutton {\n\tmargin: 0;\n\tpadding: 0;\n\tborder: 0;\n\tbackground: none;\n\tfont-size: 100%;\n\tvertical-align: baseline;\n\tfont-family: inherit;\n\tfont-weight: inherit;\n\tcolor: inherit;\n\t-webkit-appearance: none;\n\tappearance: none;\n\t-webkit-font-smoothing: antialiased;\n\t-moz-osx-font-smoothing: grayscale;\n}\n\nbody {\n\tfont: 14px 'Helvetica Neue', Helvetica, Arial, sans-serif;\n\tline-height: 1.4em;\n\tbackground: #f5f5f5;\n\tcolor: #111111;\n\tmin-width: 230px;\n\tmax-width: 550px;\n\tmargin: 0 auto;\n\t-webkit-font-smoothing: antialiased;\n\t-moz-osx-font-smoothing: grayscale;\n\tfont-weight: 300;\n}\n\n:focus {\n\toutline: 0;\n}\n\n.hidden {\n\tdisplay: none;\n}\n\n.todoapp {\n\tbackground: #fff;\n\tmargin: 130px 0 40px 0;\n\tposition: relative;\n\tbox-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2),\n\t            0 25px 50px 0 rgba(0, 0, 0, 0.1);\n}\n\n.todoapp input::-webkit-input-placeholder {\n\tfont-style: italic;\n\tfont-weight: 300;\n\tcolor: rgba(0, 0, 0, 0.4);\n}\n\n.todoapp input::-moz-placeholder {\n\tfont-style: italic;\n\tfont-weight: 300;\n\tcolor: rgba(0, 0, 0, 0.4);\n}\n\n.todoapp input::input-placeholder {\n\tfont-style: italic;\n\tfont-weight: 300;\n\tcolor: rgba(0, 0, 0, 0.4);\n}\n\n.todoapp h1 {\n\tposition: absolute;\n\ttop: -140px;\n\twidth: 100%;\n\tfont-size: 80px;\n\tfont-weight: 200;\n\ttext-align: center;\n\tcolor: #b83f45;\n\t-webkit-text-rendering: optimizeLegibility;\n\t-moz-text-rendering: optimizeLegibility;\n\ttext-rendering: optimizeLegibility;\n}\n\n.new-todo,\n.edit {\n\tposition: relative;\n\tmargin: 0;\n\twidth: 100%;\n\tfont-size: 24px;\n\tfont-family: inherit;\n\tfont-weight: inherit;\n\tline-height: 1.4em;\n\tcolor: inherit;\n\tpadding: 6px;\n\tborder: 1px solid #999;\n\tbox-shadow: inset 0 -1px 5px 0 rgba(0, 0, 0, 0.2);\n\tbox-sizing: border-box;\n\t-webkit-font-smoothing: antialiased;\n\t-moz-osx-font-smoothing: grayscale;\n}\n\n.new-todo {\n\tpadding: 16px 16px 16px 60px;\n\tborder: none;\n\tbackground: rgba(0, 0, 0, 0.003);\n\tbox-shadow: inset 0 -2px 1px rgba(0,0,0,0.03);\n}\n\n.main {\n\tposition: relative;\n\tz-index: 2;\n\tborder-top: 1px solid #e6e6e6;\n}\n\n.toggle-all {\n\twidth: 1px;\n\theight: 1px;\n\tborder: none; /* Mobile Safari */\n\topacity: 0;\n\tposition: absolute;\n\tright: 100%;\n\tbottom: 100%;\n}\n\n.toggle-all + label {\n\twidth: 60px;\n\theight: 34px;\n\tfont-size: 0;\n\tposition: absolute;\n\ttop: -52px;\n\tleft: -13px;\n\t-webkit-transform: rotate(90deg);\n\ttransform: rotate(90deg);\n}\n\n.toggle-all + label:before {\n\tcontent: '\10095';\n\tfont-size: 22px;\n\tcolor: #e6e6e6;\n\tpadding: 10px 27px 10px 27px;\n}\n\n.toggle-all:checked + label:before {\n\tcolor: #737373;\n}\n\n.todo-list {\n\tmargin: 0;\n\tpadding: 0;\n\tlist-style: none;\n}\n\n.todo-list li {\n\tposition: relative;\n\tfont-size: 24px;\n\tborder-bottom: 1px solid #ededed;\n}\n\n.todo-list li:last-child {\n\tborder-bottom: none;\n}\n\n.todo-list li.editing {\n\tborder-bottom: none;\n\tpadding: 0;\n}\n\n.todo-list li.editing .edit {\n\tdisplay: block;\n\twidth: calc(100% - 43px);\n\tpadding: 12px 16px;\n\tmargin: 0 0 0 43px;\n}\n\n.todo-list li.editing .view {\n\tdisplay: none;\n}\n\n.todo-list li .toggle {\n\ttext-align: center;\n\twidth: 40px;\n\t/* auto, since non-WebKit browsers doesn't support input styling */\n\theight: auto;\n\tposition: absolute;\n\ttop: 0;\n\tbottom: 0;\n\tmargin: auto 0;\n\tborder: none; /* Mobile Safari */\n\t-webkit-appearance: none;\n\tappearance: none;\n}\n\n.todo-list li .toggle {\n\topacity: 0;\n}\n\n.todo-list li .toggle + label {\n\t/*\n\t\tFirefox requires `#` to be escaped - https://bugzilla.mozilla.org/show_bug.cgi?id=922433\n\t\tIE and Edge requires *everything* to be escaped to render, so we do that instead of just the `#` - https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/7157459/\n\t*/\n\tbackground-image: url('data:image/svg+xml;utf8,%3Csvg%20xmlns%3D%22http%3A//www.w3.org/2000/svg%22%20width%3D%2240%22%20height%3D%2240%22%20viewBox%3D%22-10%20-18%20100%20135%22%3E%3Ccircle%20cx%3D%2250%22%20cy%3D%2250%22%20r%3D%2250%22%20fill%3D%22none%22%20stroke%3D%22%23ededed%22%20stroke-width%3D%223%22/%3E%3C/svg%3E');\n\tbackground-repeat: no-repeat;\n\tbackground-position: center left;\n}\n\n.todo-list li .toggle:checked + label {\n\tbackground-image: url('data:image/svg+xml;utf8,%3Csvg%20xmlns%3D%22http%3A//www.w3.org/2000/svg%22%20width%3D%2240%22%20height%3D%2240%22%20viewBox%3D%22-10%20-18%20100%20135%22%3E%3Ccircle%20cx%3D%2250%22%20cy%3D%2250%22%20r%3D%2250%22%20fill%3D%22none%22%20stroke%3D%22%23bddad5%22%20stroke-width%3D%223%22/%3E%3Cpath%20fill%3D%22%235dc2af%22%20d%3D%22M72%2025L42%2071%2027%2056l-4%204%2020%2020%2034-52z%22/%3E%3C/svg%3E');\n}\n\n.todo-list li label {\n\tword-break: break-all;\n\tpadding: 15px 15px 15px 60px;\n\tdisplay: block;\n\tline-height: 1.2;\n\ttransition: color 0.4s;\n\tfont-weight: 400;\n\tcolor: #4d4d4d;\n}\n\n.todo-list li.completed label {\n\tcolor: #cdcdcd;\n\ttext-decoration: line-through;\n}\n\n.todo-list li .destroy {\n\tdisplay: none;\n\tposition: absolute;\n\ttop: 0;\n\tright: 10px;\n\tbottom: 0;\n\twidth: 40px;\n\theight: 40px;\n\tmargin: auto 0;\n\tfont-size: 30px;\n\tcolor: #cc9a9a;\n\tmargin-bottom: 11px;\n\ttransition: color 0.2s ease-out;\n}\n\n.todo-list li .destroy:hover {\n\tcolor: #af5b5e;\n}\n\n.todo-list li .destroy:after {\n\tcontent: '\215';\n}\n\n.todo-list li:hover .destroy {\n\tdisplay: block;\n}\n\n.todo-list li .edit {\n\tdisplay: none;\n}\n\n.todo-list li.editing:last-child {\n\tmargin-bottom: -1px;\n}\n\n.footer {\n\tpadding: 10px 15px;\n\theight: 20px;\n\ttext-align: center;\n\tfont-size: 15px;\n\tborder-top: 1px solid #e6e6e6;\n}\n\n.footer:before {\n\tcontent: '';\n\tposition: absolute;\n\tright: 0;\n\tbottom: 0;\n\tleft: 0;\n\theight: 50px;\n\toverflow: hidden;\n\tbox-shadow: 0 1px 1px rgba(0, 0, 0, 0.2),\n\t            0 8px 0 -3px #f6f6f6,\n\t            0 9px 1px -3px rgba(0, 0, 0, 0.2),\n\t            0 16px 0 -6px #f6f6f6,\n\t            0 17px 2px -6px rgba(0, 0, 0, 0.2);\n}\n\n.todo-count {\n\tfloat: left;\n\ttext-align: left;\n}\n\n.todo-count strong {\n\tfont-weight: 300;\n}\n\n.filters {\n\tmargin: 0;\n\tpadding: 0;\n\tlist-style: none;\n\tposition: absolute;\n\tright: 0;\n\tleft: 0;\n}\n\n.filters li {\n\tdisplay: inline;\n}\n\n.filters li a {\n\tcolor: inherit;\n\tmargin: 3px;\n\tpadding: 3px 7px;\n\ttext-decoration: none;\n\tborder: 1px solid transparent;\n\tborder-radius: 3px;\n}\n\n.filters li a:hover {\n\tborder-color: rgba(175, 47, 47, 0.1);\n}\n\n.filters li a.selected {\n\tborder-color: rgba(175, 47, 47, 0.2);\n}\n\n.clear-completed,\nhtml .clear-completed:active {\n\tfloat: right;\n\tposition: relative;\n\tline-height: 20px;\n\ttext-decoration: none;\n\tcursor: pointer;\n}\n\n.clear-completed:hover {\n\ttext-decoration: underline;\n}\n\n.info {\n\tmargin: 65px auto 0;\n\tcolor: #4d4d4d;\n\tfont-size: 11px;\n\ttext-shadow: 0 1px 0 rgba(255, 255, 255, 0.5);\n\ttext-align: center;\n}\n\n.info p {\n\tline-height: 1;\n}\n\n.info a {\n\tcolor: inherit;\n\ttext-decoration: none;\n\tfont-weight: 400;\n}\n\n.info a:hover {\n\ttext-decoration: underline;\n}\n\n/*\n\tHack to remove background from Mobile Safari.\n\tCan't use it globally since it destroys checkboxes in Firefox\n*/\n@media screen and (-webkit-min-device-pixel-ratio:0) {\n\t.toggle-all,\n\t.todo-list li .toggle {\n\t\tbackground: none;\n\t}\n\n\t.todo-list li .toggle {\n\t\theight: 40px;\n\t}\n}\n\n@media (max-width: 430px) {\n\t.footer {\n\t\theight: 50px;\n\t}\n\n\t.filters {\n\t\tbottom: 10px;\n\t}\n}\n"

todoMVC
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => m ()
todoMVC = el "div" $ do
  elAttr "section" ("class" =: "todoapp") $ do
    mainHeader
    rec tasks <- foldDyn ($) initialTasks $ mergeWith (.)
                   [ fmap insertNew_ newTask
                   , listModifyTasks
                   , fmap (const $ Map.filter $ not . taskCompleted) clearCompleted -- Call out the type and purpose of these things
                   ]
        newTask <- taskEntry
        listModifyTasks <- taskList activeFilter tasks
        (activeFilter, clearCompleted) <- controls tasks
    return ()
  infoFooter

-- | Display the main header
mainHeader :: DomBuilder t m => m ()
mainHeader = el "h1" $ text "todos"

-- | Strip leading and trailing whitespace from the user's entry, and discard it if nothing remains
stripDescription :: Text -> Maybe Text
stripDescription d =
  let trimmed = T.strip d
  in if T.null trimmed
     then Nothing
     else Just trimmed

keyCodeIs :: Key -> KeyCode -> Bool
keyCodeIs k c = keyCodeLookup c == k

-- | Display an input field; produce new Tasks when the user creates them
taskEntry
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     )
  => m (Event t Task)
taskEntry = el "header" $ do
  -- Create the textbox; it will be cleared whenever the user presses enter
  rec let newValueEntered = keypress Enter descriptionBox
      descriptionBox <- inputElement $ def
        & inputElementConfig_setValue .~ fmap (const "") newValueEntered
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
            mconcat [ "class" =: "new-todo"
                    , "placeholder" =: "What needs to be done?"
                    , "name" =: "newTodo"
                    , "type" =: "text"
                    ]
  -- -- Request focus on this element when the widget is done being built
  -- schedulePostBuild $ liftIO $ focus $ _textInput_element descriptionBox
  let -- | Get the current value of the textbox whenever the user hits enter
      newValue = tag (current $ value descriptionBox) newValueEntered
  -- -- Set focus when the user enters a new Task
  -- performEvent_ $ fmap (const $ liftIO $ focus $ _textInput_element descriptionBox) newValueEntered
  return $ fmap (\d -> Task d False) $ fmapMaybe stripDescription newValue

-- | Display the user's Tasks, subject to a Filter; return requested modifications to the Task list
taskList
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Ord k
     )
  => Dynamic t Filter
  -> Dynamic t (Map k Task)
  -> m (Event t (Map k Task -> Map k Task))
taskList activeFilter tasks = elAttr "section" ("class" =: "main") $ do
  let toggleAllState = all taskCompleted . Map.elems <$> tasks
      toggleAllAttrs = ffor tasks $ \t -> "class" =: "toggle-all" <> "name" =: "toggle" <> if Map.null t then "style" =: "visibility:hidden" else mempty
  toggleAll <- toggleInput toggleAllAttrs toggleAllState
  elAttr "label" ("for" =: "toggle-all") $ text "Mark all as complete"
  -- Filter the item list
  let visibleTasks = zipDynWith (Map.filter . satisfiesFilter) activeFilter tasks
  -- Hide the item list itself if there are no items
  let itemListAttrs = ffor visibleTasks $ \t -> mconcat
        [ "class" =: "todo-list"
        , if Map.null t then "style" =: "visibility:hidden" else mempty
        ]
  -- Display the items
  items <- elDynAttr "ul" itemListAttrs $ list visibleTasks todoItem
  -- Aggregate the changes produced by the elements
  let combineItemChanges = fmap (foldl' (.) id) . mergeList . map (\(k, v) -> fmap (flip Map.update k) v) . Map.toList
      itemChangeEvent = fmap combineItemChanges items
      itemChanges = switch $ current itemChangeEvent
  return itemChanges

toggleInput
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t (Map AttributeName Text)
  -> Dynamic t Bool
  -> m (Event t ())
toggleInput dynAttrs dynChecked = do
  let attrs = (<> "class" =: "toggle") . ("type" =: "checkbox" <>) <$> dynAttrs
      updatedAttrs = fmap Just <$> updated dynAttrs
      updatedChecked = updated dynChecked
  initialAttrs <- sample $ current attrs
  initialChecked <- sample $ current dynChecked
  domEvent Click <$> inputElement (def
    & inputElementConfig_initialChecked .~ initialChecked
    & inputElementConfig_setChecked .~ updatedChecked
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ updatedAttrs
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initialAttrs)

buildCompletedCheckbox
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t Task
  -> Dynamic t Text
  -> m (Event t Bool, Event t (), Event t ())
buildCompletedCheckbox todo description = elAttr "div" ("class" =: "view") $ do
  -- Display the todo item's completed status, and allow it to be set
  completed <- holdUniqDyn $ fmap taskCompleted todo
  checkboxClicked <- toggleInput (constDyn mempty) completed
  let setCompleted = fmap not $ tag (current completed) checkboxClicked
  -- Display the todo item's name for viewing purposes
  (descriptionLabel, _) <- el' "label" $ dynText description
  -- Display the button for deleting the todo item
  (destroyButton, _) <- elAttr' "button" ("class" =: "destroy") $ return ()
  return ( setCompleted
         , domEvent Click destroyButton
         , void $ domEvent Dblclick descriptionLabel
         )

-- | Display an individual todo item
todoItem
  :: ( DomBuilder t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t Task
  -> m (Event t (Task -> Maybe Task))
todoItem todo = do
  description <- holdUniqDyn $ fmap taskDescription todo
  rec -- Construct the attributes for our element
      let attrs = ffor2 todo editing' $ \t e -> Map.singleton "class" $ T.unwords $ concat
            [ [ "completed" | taskCompleted t ]
            , [ "editing" | e ]
            ]
      (editing', changeTodo) <- elDynAttr "li" attrs $ do
        (setCompleted, destroy, startEditing) <- buildCompletedCheckbox todo description
        -- Set the current value of the editBox whenever we start editing (it's not visible in non-editing mode)
        let setEditValue = tag (current description) $ ffilter id $ updated editing'
        editBox <- inputElement $ def
          & inputElementConfig_setValue .~ setEditValue
          & inputElementConfig_elementConfig . elementConfig_initialAttributes
            .~ ("class" =: "edit" <> "name" =: "title")
        let -- Set the todo item's description when the user leaves the textbox or presses enter in it
            setDescription = tag (current $ value editBox) $ leftmost
              [ keypress Enter editBox
              , domEvent Blur editBox
              ]
            -- Cancel editing (without changing the item's description) when the user presses escape in the textbox
            cancelEdit = keypress Escape editBox
            -- Put together all the ways the todo item can change itself
            changeSelf = mergeWith (>=>) [ fmap (\c t -> Just $ t { taskCompleted = c }) setCompleted
                                         , fmap (const $ const Nothing) destroy
                                         , fmap (\d t -> fmap (\trimmed -> t { taskDescription = trimmed }) $ stripDescription d) setDescription
                                         ]
        -- Set focus on the edit box when we enter edit mode
--        postGui <- askPostGui
--        performEvent_ $ fmap (const $ liftIO $ void $ forkIO $ threadDelay 1000 >> postGui (liftIO $ focus $ _textInput_element editBox)) startEditing -- Without the delay, the focus doesn't take effect because the element hasn't become unhidden yet; we need to use postGui to ensure that this is threadsafe when built with GTK
        -- Determine the current editing state; initially false, but can be modified by various events
        editing <- holdDyn False $ leftmost [ fmap (const True) startEditing
                                            , fmap (const False) setDescription
                                            , fmap (const False) cancelEdit
                                            ]
        return (editing, changeSelf)
  -- Return an event that fires whenever we change ourselves
  return changeTodo

buildActiveFilter
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => m (Dynamic t Filter)
buildActiveFilter = elAttr "ul" ("class" =: "filters") $ do
  rec activeFilter <- holdDyn All setFilter
      let filterButton f = el "li" $ do
            let buttonAttrs = ffor activeFilter $ \af -> "class" =: if f == af then "selected" else ""
            (e, _) <- elDynAttr' "a" buttonAttrs $ text $ T.pack $ show f
            return $ fmap (const f) (domEvent Click e)
      allButton <- filterButton All
      text " "
      activeButton <- filterButton Active
      text " "
      completedButton <- filterButton Completed
      let setFilter = leftmost [allButton, activeButton, completedButton]
  return activeFilter

-- | Display the control footer; return the user's currently-selected filter and an event that fires when the user chooses to clear all completed events
controls
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t (Map k Task)
  -> m (Dynamic t Filter, Event t ())
controls tasks = do
  -- Determine the attributes for the footer; it is invisible when there are no todo items
  let controlsAttrs = ffor tasks $ \t -> "class" =: "footer" <> if Map.null t then "style" =: "visibility:hidden" else mempty
  elDynAttr "footer" controlsAttrs $ do
    -- Compute the number of completed and uncompleted tasks
    let (tasksCompleted, tasksLeft) = splitDynPure $ ffor tasks $ \m ->
          let completed = Map.size $ Map.filter taskCompleted m
          in (completed, Map.size m - completed)
    elAttr "span" ("class" =: "todo-count") $ do
      el "strong" $ dynText $ fmap (T.pack . show) tasksLeft
      dynText $ fmap (\n -> (if n == 1 then " item" else " items") <> " left") tasksLeft
    activeFilter <- buildActiveFilter
    let clearCompletedAttrs = ffor tasksCompleted $ \n -> mconcat
          [ "class" =: "clear-completed"
          , if n > 0 then mempty else "hidden" =: ""
          ]
    (clearCompletedAttrsButton, _) <- elDynAttr' "button" clearCompletedAttrs $ dynText $ ffor tasksCompleted $ \n -> "Clear completed (" <> T.pack (show n) <> ")"
    return (activeFilter, domEvent Click clearCompletedAttrsButton)

-- | Display static information about the application
infoFooter :: DomBuilder t m => m ()
infoFooter = elAttr "footer" ("class" =: "info") $ do
  el "p" $ text "Click to edit a todo"
  el "p" $ do
    text "Written by "
    elAttr "a" ("href" =: "https://github.com/ryantrinkle") $ text "Ryan Trinkle"
  el "p" $ do
    text "Part of "
    elAttr "a" ("href" =: "http://todomvc.com") $ text "TodoMVC"
