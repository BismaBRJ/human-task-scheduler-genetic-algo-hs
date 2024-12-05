{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom
import qualified Data.Text as T
import           MyBackend
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

main :: IO ()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement =  el "div" $ do
    -- memasukkan HTML element dengan fungsi "el",
    -- diikuti misal h1 (heading) atau p (paragraf),
    -- lalu dolar,
    -- lalu fungsi "text" untuk text box
    el "h1" $ text "Human Task Scheduler v1.0.0 (brute force)"
    el "p" $ text "Input work hours and tasks to obtain your task schedule"

    -- input kalender
    el "h2" $ text "Calendar (work hours)"
    el "p" $ text "Input your work hours"
    el "p" $ text "Format: [(day1,hour1), (day2,hour2), ...] where"
    el "ul" $ do
        el "li" $ text "\"day\" is a positive integer"
        el "li" $ text "\"hour\" is a positive integer"
    el "p" $ text "Example (two days): [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4)]"
    workingHoursInput <- textArea def
    let cal = fromMaybe tempDayHours . readMaybe . T.unpack <$> value workingHoursInput
    el "p" $ text "Value received:"
    --dynText $ value workingHoursInput
    display cal
    
    -- input full hours
    el "h2" $ text "Full hours (unavailable times)"
    el "p" $ text "Input full hours; unavailable time slots of your calendar"
    el "p" $ text "Format: [(day1,hour1), (day2,hour2), ...] where"
    el "ul" $ do
        el "li" $ text "\"day\" is a positive integer"
        el "li" $ text "\"hour\" is a positive integer"
    el "p" $ text "Example (two hours in day one, one hour in day two): [(1,3),(1,4),(2,1)]"
    fullHoursInput <- textArea def
    let kls = fromMaybe tempDayHours . readMaybe . T.unpack <$> value fullHoursInput
    el "p" $ text "Value received:"
    --dynText $ value fullHoursInput
    display kls

    -- input tugas
    el "h2" $ text "Tasks"
    el "p" $ text "Input your tasks as well as deadlines"
    el "p" $ text "Format: [(name, (day,hour), hours), ...] where"
    el "ol" $ do
        el "li" $ text "\"name\" is text"
        el "li" $ text "\"day\" and \"hour\" (task deadline) are positive integers"
        el "li" $ text "\"hours\" (task workload) is a positive integer"
    el "p" $ text "Example: [(\"task1\", (1,4), 1), (\"task2\", (2,3), 2)]"
    taskListInput <- textArea def
    let hts = fromMaybe tempHumanTasks . readMaybe . T.unpack <$> value taskListInput
    el "p" $ text "Value received:"
    --dynText $ value taskListInput
    display hts

    -- menuju hasil
    --let res = zipDynWith4 scheduleTasks cal kls hts
    let res = ffor3 cal kls hts scheduleTasks

    -- hasil
    el "h2" $ text "Result"
    el "p" $ text "Here is your result: "
    display res
    el "p" $ text "Format: [(task1 details, (daytodo,hourtodo)), (task2 details, (daytodo,hourtodo)), ...] where"
    el "ul" $ do
        el "li" $ text "\"task details\" consist of the usual format: (name, (day,hour), hours)"
        el "li" $ text "(daytodo,hourtodo) is an allocated time slot to do the particular task"
        el "li" $ text "A task spanning multiple hours is allotted to multiple time slots"
    
    el "h2" $ text "Other tests"

    -- some table
    el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" $ text "Tugas"
                el "th" $ text "Deadline"
        el "tbody" $ do
            el "tr" $ do
                elAttr "td" ("contenteditable" =: "true") $ text "Row 1, Col 1"
                elAttr "td" ("contenteditable" =: "true") $ text "Row 1, Col 2"
            el "tr" $ do
                elAttr "td" ("contenteditable" =: "true") $ text "Row 2, Col 1"
                elAttr "td" ("contenteditable" =: "true") $ text "Row 2, Col 2"

    {-
    rec
        dynRows <- foldDyn applyAction initialRows addRemoveEv

        el "table" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Task"
                    el "th" $ text "Deadline"
                    el "th" $ text "Remove"
            el "tbody" $ do
                removeEvList <- simpleList dynRows renderRow
                let removeEv = switchDyn $ leftmost <$> removeEvList

        addEv <- button "Add Row"
        let addRemoveEv = leftmost [AddRow <$ addEv, RemoveRow <$> switchDyn removeEv]
    -}

    -- referensi sementara
    el "h3" $ text "Simple Text Input"
    ti <- textInput def
    dynText $ value ti

    el "p" $ text "This static webpage was last updated on: Thursday, 5 December 2024"

{-
data RowAction = AddRow | RemoveRow Int

applyAction :: RowAction -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
applyAction AddRow rows = rows ++ [("New Col 1", "New Col 2")]
applyAction (RemoveRow idx) rows = take idx rows ++ drop (idx + 1) rows

initialRows :: [(T.Text, T.Text)]
initialRows = [("Row 1, Col 1", "Row 1, Col 2"), ("Row 2, Col 1", "Row 2, Col 2")]

renderRow :: MonadWidget t m => Dynamic t (T.Text, T.Text) -> m (Event t RowAction)
renderRow row = do
    el "tr" $ do
        -- Editable columns
        elAttr "td" ("contenteditable" =: "true") $ dynText (fst <$> row)
        elAttr "td" ("contenteditable" =: "true") $ dynText (snd <$> row)

        -- Remove button
        (removeBtn, _) <- el' "td" $ button "Remove"
        return $ RemoveRow <$> currentIndex removeBtn

currentIndex :: DomBuilder t m => El t -> Event t Int
currentIndex el = tag (current $ zipDynWith const (pure 0) $ constDyn 0) $ domEvent Click el
-}

{-
zipDynWith4 :: (a -> b -> c -> d) -> Dynamic t a -> Dynamic t b -> Dynamic t c -> Dynamic t d
zipDynWith4 f da db dc = zipDynWith (\(a, b) c -> f a b c) (zipDynWith (,) da db) dc
-}