import MyBackend

-- import untuk frontend

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex.Dom
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- frontend: progress 02

{-
-- untuk scheduling tapi harus teks?
dummyScheduleText :: [Text] -> [Text] -> [Text]
dummyScheduleText _ _ = ["ScheduledTask 1", "ScheduledTask 2"]

frontend :: (DomBuilder t m, PostBuild t m) => m ()
frontend = mainUI

mainUI :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
mainUI = do
    -- memasukkan HTML element dengan fungsi "el",
    -- diikuti misal h1 (heading) atau p (paragraf),
    -- lalu fungsi "text" untuk text box
    el "h1" $ text "Human Task Scheduler v0.2.0 (brute force)"
    el "p" $ text "Masukkan jam kerja dan tugas untuk menghasilkan jadwal pengerjaan tugas"

    -- input kalender
    el "h2" $ text "Jam kerja"
    workingHoursInput <- textInput def
    el "p" $ text "Masukkan jam kerja"

    -- input tugas
    el "h2" $ text "Masukkan tugas beserta deadline"
    taskList <- inputTaskList

    -- tombol generate schedule
    generateButton <- button "Jadwalkan"
    let taskNames = fmap (map fst) taskList
        deadlines = fmap (map snd) taskList

    -- tampilkan
    schedule <- foldDyn (const . uncurry dummyScheduleText) [] (tag (current taskNames) (current deadlines) <@ generateButton)
    displayScheduleTable schedule

-- fungsi untuk tabel menginput tugas
inputTaskList :: (DomBuilder t m, MonadHold t m) => m (Dynamic t [(Text, Text)])
inputTaskList = do
    rec
      let blankRow = ("Nama Tugas", "Deadline")
      newRow <- button "Tambahkan tugas"
      tasks <- foldDyn (const (++ [blankRow])) [blankRow] newRow
      taskInputs <- simpleList tasks (uncurry taskRow)
    return $ joinDynThroughMap taskInputs

-- baris di tabel list tugas
taskRow :: (DomBuilder t m, MonadHold t m) => Text -> Text -> m (Dynamic t (Text, Text))
taskRow taskName deadline = do
    t <- textInput $ def & textInputConfig_initialValue .~ taskName
    d <- textInput $ def & textInputConfig_initialValue .~ deadline
    return $ zipDynWith (,) (_textInput_value t) (_textInput_value d)

-- fungsi untuk render tabel jadwal hasil
displayScheduleTable :: DomBuilder t m => Dynamic t [Text] -> m ()
displayScheduleTable schedule = do
    el "h2" $ text "Jadwal pengerjaan tugas"
    el "table" $ do
      el "tr" $ do
        el "th" $ text "Jam"
        el "th" $ text "Tugas terjadwal"
      simpleList schedule $ \task -> el "tr" $ el "td" $ dynText task
-}

-- progress 04: rework untuk brute force dulu

maybeToText :: Maybe Double -> Text
maybeToText = pack . maybe "" show

main :: IO ()
main = mainWidget $ el "div" $ mdo
  el "h1" $ text "Kalikan tiga bilangan"
  
  -- input
  n1 <- numberInput "Angka 1"
  n2 <- numberInput "Angka 2"
  n3 <- numberInput "Angka 3"

  let productDyn = zipDynWith (*) n1 =<< zipDynWith (*) n2 n3

  el "h2" $ text "Hasil kali"
  display productDyn

numberInput :: (DomBuilder t m, MonadHold t m, PostBuild t m) => Text -> m (Dynamic t Double)
numberInput label = do
  el "label" $ text label
  n <- inputElement $ def
    & inputElementConfig_initialValue .~ "1"  -- default
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
  return $ fmap (maybe 1 id . readMaybe . unpack) (_inputElement_value n)
