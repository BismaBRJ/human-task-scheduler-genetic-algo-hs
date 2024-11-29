{-
sumber belajar:

https://reflex-frp.org/resources

https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md

https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md
-}

--import MyBackend

-- import untuk frontend

{-
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE FlexibleContexts #-}

-- import Reflex
import Reflex.Dom

-- di Reflex digunakan tipe data Text daripada String
import qualified Data.Text as T

import qualified Data.Map as Map

--import Data.Monoid

{-
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
-}

-}

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

{-
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
-}

-- progress 05: coba lagi

--{-# LANGUAGE OverloadedStrings #-}

--import Reflex.Dom
--main :: IO ()
--main = mainWidget $ display =<< count =<< button "ClickMe"

{-
import Reflex.Dom

main :: IO ()
main = do
  mainWidget $ display (count (button "ClickMe"))
-}

--import Reflex.Dom
--main :: IO ()
--main = mainWidget $ el "h1" $ text "Welcome to Reflex-Dom"

{-
import Reflex.Dom
main :: IO ()
main = mainWidget $ do
  el "h1" $ text "Welcome to Reflex-Dom"
  el "div" $ do
    el "p" $ text "Reflex-Dom is:"
    el "ul" $ do
      el "li" $ text "Fun"
      el "li" $ text "Not difficult"
      el "li" $ text "Efficient"
  el "br" blank --el "br" $ return ()
-}

{-
import Reflex.Dom
import qualified Data.Text as T 
import qualified Data.Map as Map
import           Data.Monoid ((<>))

main :: IO ()
main = mainWidget $ do
    el "h1" $ text "A link to Google in a new tab"
    elAttr "a" attrs $ text "Google!"

attrs :: Map.Map T.Text T.Text
attrs = ("target" =: "_blank") <> ("href" =: "http://google.com")
-}
-- buat satu pemetaan dengan =: dari Reflex.Dom
-- gabungkan dua pemetaan dengan <>

--{-# LANGUAGE RecursiveDo #-}
-- agar bisa menggunakan fungsi, misal evClick, sebelum terdefinisi
-- (sebagaimana fungsi di Haskell pada umumnya,
-- definisi bisa setelah / di bawah penggunaan)

{-
import           Reflex.Dom
import qualified Data.Text as T 
import qualified Data.Map as Map
import           Data.Monoid ((<>))
main :: IO ()
main = mainWidget $ do
  rec
    dynBool <- toggle False evClick
    let dynAttrs = attrs <$> dynBool
    elDynAttr "h1" dynAttrs $ text "Changing color"
    evClick <- button "Change Color"
  return ()
attrs :: Bool -> Map.Map T.Text T.Text
attrs b = "style" =: ("color: " <> color b)
  where 
    color True = "red"
    color _    = "green"
-}

{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
main :: IO()
main = mainWidget bodyElement 

bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  el "h2" $ text "Simple Text Input"
  ti <- textInput def
  dynText $ value ti

{-
bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  el "h1" $ text "Human Task Scheduler (brute force)"
  el "br" blank --el "br" $ return ()
  el "div" $ do
    el "h2" $ text "Masukkan kalender jam kerja:"
    el "br" blank
    el "p" $ text "Format: [(hari (bilangan bulat positif), jam kerja)]"
    el "br" blank
    in1 <- textInput def
    el "h2" $ text "Masukkan jam-jam penuh:"
    el "br" blank
    el "p" $ text "Format: [(hari (bilangan bulat positif), jam penuh)]"
    el "br" blank
    in2 <- textInput def
    el "h2" $ text "Masukkan list tugas:"
    el "br" blank
    el "p" $ text "Format: [(\"nama tugas\", (hari, jam deadline), waktu pengerjaan)]"
    el "br" blank
    in3 <- textInput def

    in1 ++ in2 ++ in3
-}

{-
bodyElement :: MonadWidget t m => m ()
bodyElement = el (fromString "div") $ do
  el (fromString "h1") $ text (fromString "Human Task Scheduler (brute force)")
  el (fromString "br") blank --el "br" $ return ()
  el (fromString "div") $ do
    el (fromString "h2") $ text (fromString "Masukkan kalender jam kerja:")
    el (fromString "br") blank
    el (fromString "p") $ text (fromString "Format: [(hari (bilangan bulat positif), jam kerja)]")
    el (fromString "br") blank
    in1 <- textInput def
    el (fromString "h2") $ text (fromString "Masukkan jam-jam penuh:")
    el (fromString "br") blank
    el (fromString "p") $ text (fromString "Format: [(hari (bilangan bulat positif), jam penuh)]")
    el (fromString "br") blank
    in2 <- textInput def
    el (fromString "h2") $ text (fromString "Masukkan list tugas:")
    el (fromString "br") blank
    el (fromString "p") $ text (fromString "Format: [(\"nama tugas\", (hari, jam deadline), waktu pengerjaan)]")
    el (fromString "br") blank
    in3 <- textInput def

    in1 <> in2 <> in3
-}

{-
bodyElement :: MonadWidget t m => m ()
bodyElement = el "div" $ do
  el "h1" $ text "Human Task Scheduler (brute force)"
  el "br" blank --el "br" $ return ()
  el "h2" $ text "Masukkan kalender jam kerja:"
  el "br" blank
  el "p" $ text "Format: [(hari (bilangan bulat positif), jam kerja)]"
  el "br" blank
  
  el "h2" $ text "Masukkan jam-jam penuh:"
  el "br" blank
  el "p" $ text "Format: [(hari (bilangan bulat positif), jam penuh)]"
  el "br" blank
  el "h2" $ text "Masukkan list tugas:"
  el "br" blank
  el "p" $ text "Format: [(\"nama tugas\", (hari, jam deadline), waktu pengerjaan)]"
  el "br" blank
-}


{-
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE RecursiveDo #-}
--{-# LANGUAGE ScopedTypeVariables #-}
import           Reflex.Dom
--import qualified Data.Text as T 
--import qualified Data.Map as Map
--import           Data.Monoid ((<>))
main :: IO()
main = mainWidget $ do
  el "h1" $ text "Human Task Scheduler (brute force)"
{-
  el "br" blank --el "br" $ return ()
  el "h2" $ text "Masukkan kalender jam kerja:"
  el "br" blank
  el "p" $ text "Format: [(hari (bilangan bulat positif), jam kerja)]"
  el "br" blank
  
  el "h2" $ text "Masukkan jam-jam penuh:"
  el "br" blank
  el "p" $ text "Format: [(hari (bilangan bulat positif), jam penuh)]"
  el "br" blank
  el "h2" $ text "Masukkan list tugas:"
  el "br" blank
  el "p" $ text "Format: [(\"nama tugas\", (hari, jam deadline), waktu pengerjaan)]"
  el "br" blank
-}
-}

{-
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE RecursiveDo #-}
--{-# LANGUAGE ScopedTypeVariables #-}

import Reflex.Dom

--import qualified Data.Text as T 
--import qualified Data.Map as Map
--import           Data.Monoid ((<>))
main :: IO()
main = mainWidget $ do
  el "h1" $ text "Human Task Scheduler (brute force)"
  el "br" blank --el "br" $ return ()
  el "h2" $ text "Masukkan kalender jam kerja:"
  el "br" blank
  el "p" $ text "Format: [(hari (bilangan bulat positif), jam kerja)]"
  el "br" blank
  
  el "h2" $ text "Masukkan jam-jam penuh:"
  el "br" blank
  el "p" $ text "Format: [(hari (bilangan bulat positif), jam penuh)]"
  el "br" blank
  el "h2" $ text "Masukkan list tugas:"
  el "br" blank
  el "p" $ text "Format: [(\"nama tugas\", (hari, jam deadline), waktu pengerjaan)]"
  el "br" blank
-}