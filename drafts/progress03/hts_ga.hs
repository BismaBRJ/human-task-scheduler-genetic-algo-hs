-- import untuk backend

import Data.List (sort, sortBy, nub, maximumBy)
import Data.Function (on)

import Control.Monad (replicateM)

-- random
import System.Random (randomRIO)

-- import untuk frontend

{-
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
-}

-- === PART 1: URUSAN JAM ===

type HourNum = Int

-- arbitrer
{-
hoursPerDay :: HourNum
hoursPerDay = 24
firstWorkHour :: HourNum
firstWorkHour = 8
lastWorkHour :: HourNum
lastWorkHour = 17
-}

--getWorkingHoursInterval :: Int -> Int -> [String]
--getWorkingHoursInterval a b = [show x | x <- [a..b]]
getWorkHoursNum :: HourNum -> HourNum -> [HourNum]
getWorkHoursNum a b = [a..b]
getWorkHoursStr :: HourNum -> HourNum -> [String]
getWorkHoursStr a b = map show (getWorkHoursNum a b)

{-
-- arbitrer
workHoursStr :: [String]
-- --workingHours = [show x | x <- [firstWorkingHour..lastWorkingHour]]
workHoursStr = getWorkHoursStr firstWorkHour lastWorkHour
-}

-- === PART 2: URUSAN HARI ===

type DayNum = Int

-- sistem penanggalan numerik

getDaysNum :: DayNum -> DayNum -> [DayNum]
getDaysNum a b = [a..b]

getDaysNumStr :: DayNum -> DayNum -> [String]
getDaysNumStr a b = map show (getDaysNum a b)

--getDays :: Int -> Int -> [String]
--getDays a b = map ("Hari ke-"++) (getWorkingHours a b)

-- arbitrer
firstDayNum :: DayNum
firstDayNum = 1

-- sistem penanggalan pekan, hari (arbitrer)

type WeekNum = Int

data Day
    = Ahad | Senin | Selasa | Rabu | Kamis | Jumat | Sabtu
    deriving (Show, Eq, Ord, Enum)

type WeekAndDay = (WeekNum, Day)

--sortDays :: [DayAndWeek] -> [DayAndWeek]
--sortDays = sort

weekAndDayToNum :: WeekAndDay -> DayNum
weekAndDayToNum (a,b) = (a-1) * 7 + fromEnum b + 1

weekAndDayFromNum :: DayNum -> WeekAndDay
weekAndDayFromNum x = (1 + div (x-1) 7, toEnum (mod (x-1) 7))

-- === PART 3: JADWAL KOSONG/TIDAK, JAM-HARI ===

getPairsObjIdx :: [a] -> [(a, Int)]
getPairsObjIdx xs = zip xs [1..(length xs)]

revTuplesInList :: [(a, b)] -> [(b, a)]
revTuplesInList tups = [(snd tup, fst tup) | tup <- tups]

listToMaybeFunction :: (Eq a, Eq b) => [(a, b)] -> (a -> Maybe b)
listToMaybeFunction ps = func
    where
        func x  | null allMatching              = Nothing
                | head allMatching `elem` ps 
                    = Just (snd (head allMatching))
                | otherwise                     = Nothing
            where
                allMatching = [pair | pair <- ps, fst pair == x]

type DayHour = (DayNum, HourNum)

type WorkCalendar = [DayHour]
type FullHours = [DayHour]
type DayHourNum = Int

getDayHourToFromNum
    :: WorkCalendar -> FullHours
    -> (DayHour -> Maybe DayHourNum, DayHourNum -> Maybe DayHour)
getDayHourToFromNum wcInput fhInput = (f, fInv)
    where
        wc = (nub . sort) wcInput
        fh = (nub . sort) fhInput
        filtered = filter (`notElem` fh) wc
        listDayHourIdx = getPairsObjIdx filtered
        f = listToMaybeFunction listDayHourIdx
        fInv = (listToMaybeFunction . revTuplesInList) listDayHourIdx

{-
    WorkCalendar: [DayHour]
    (1,1), (1,2), (1,3), (2,1), (2,2), (2,3)

    FullHours:
           (1,2), (1,3)

    DayHourNum:
    1                    2      3      4
-}

-- utility functions to construct a WorkCalendar

getDayHourInterval :: DayHour -> DayHour -> WorkCalendar
getDayHourInterval p1 p2
    = [(x,y) | x <- [(fst p1)..(fst p2)], y <- [(snd p1)..(snd p2)]]

{-
main :: IO()
main = do
    let wc = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]
    -- or: let wc = getDayHourInterval (1,1) (2,3)
    let fh = [(1,2),(1,3)]
    let myMaps = getDayHourToFromNum wc fh
    let myF = fst myMaps
    let myFInv = snd myMaps
    print (map myF wc)
    print (map myFInv [1..10])
-}

testF30Okt2024 :: [Maybe DayHourNum]
testF30Okt2024 = map myF wc
    where
        wc = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]
        -- sama saja: wc = getDayHourInterval (1,1) (2,3)
        fh = [(1,2),(1,3)]
        myMaps = getDayHourToFromNum wc fh
        myF = fst myMaps

testFInv30Okt2024 :: [Maybe DayHour]
testFInv30Okt2024 = map myFInv [1..10]
    where
        wc = getDayHourInterval (1,1) (2,3)
        fh = [(1,2),(1,3)]
        myMaps = getDayHourToFromNum wc fh
        myFInv = snd myMaps

-- === PART 4: STRUKTUR DATA TUGAS ===

---- AP: After Present
---- relatif terhadap masa kini
--data HumanTask = HumanTask {
--    taskName :: String,
--    deadlineAP :: HourDayNum
--    -- deadlineAP berupa beberapa satuan waktu (bulat),
--    -- mengikuti sistem penanggalan numerik (di atas)
--} deriving (Show)
-- type HumanTasks = [HumanTask]

--newTask :: String -> Int -> HumanTask
--newTask s i = HumanTask { taskName = s, deadlineAP = i }

type TaskName = String
type Deadline = DayHour
type DeadlineNum = DayHourNum
type Duration = Int
type HumanTask = (TaskName, Deadline, Duration)

{-
getHtTn :: HumanTask -> TaskName
getHtTn (tn,dl,dur) = tn

getHtDl :: HumanTask -> Deadline
getHtDl (tn,dl,dur) = dl

getHtDur :: HumanTask -> Duration
getHtDur (tn,dl,dur) = dur
-}

fstTriplet :: (a,b,c) -> a
fstTriplet (x,y,z) = x
sndTriplet :: (a,b,c) -> b
sndTriplet (x,y,z) = y
trdTriplet :: (a, b, c) -> c
trdTriplet (x,y,z) = z

--type HumanTaskNum = (TaskName, DeadlineNum, Duration)
{-
getHumanTaskToNum :: WorkCalendar -> FullHours
    -> (HumanTask -> HumanTaskNum)
    getHumanTaskToNum wc fh = f
        where
            f (nam,dl,dur) = (nam,dlnum,dur)
                where
                    gs = getDayHourToFromNum wc fh
                    dlnum = 
-}

sortTasks :: [HumanTask] -> [HumanTask]
sortTasks = sortBy f
    where
        f (tn1,dl1,dur1) (tn2,dl2,dur2)
            = compare (dl1,dur1,tn1) (dl2,dur2,tn2)

newtype ExpandedTask = Expanded HumanTask
    deriving (Show, Eq)

type ScheduledTask = (HumanTask, DayHour)
type ScheduledTaskNum = (HumanTask, DayHourNum)

expandTaskList :: [HumanTask] -> [ExpandedTask]
expandTaskList hts
    = concat [replicate (trdTriplet ht) (Expanded ht) | ht <- hts]

getDeadlineViolations :: [ScheduledTask] -> [ScheduledTask]
getDeadlineViolations stsInput = reverse backwardsResult
    where
        backwardsSts = (nub . reverse) stsInput
        backwardsResult
            = [ st | st <- backwardsSts,
                sndTriplet (fst st) < snd st ]

dhJustOrZero :: Maybe DayHour -> DayHour
dhJustOrZero (Just x)   = x
dhJustOrZero Nothing    = (0,0)

getDeadlineViolationsMaybe :: [(HumanTask, Maybe DayHour)]
    -> [(HumanTask, Maybe DayHour)]
getDeadlineViolationsMaybe stsInput = reverse backwardsResult
    where
        backwardsSts = (nub . reverse) stsInput
        backwardsResult
            = [ st | st <- backwardsSts,
                sndTriplet (fst st) < dhJustOrZero (snd st) ]

checkDeadlineOrder :: [(HumanTask, DayHour)] -> Bool
checkDeadlineOrder = null . getDeadlineViolations

checkDeadlineOrderMaybe :: [(HumanTask, Maybe DayHour)] -> Bool
checkDeadlineOrderMaybe = null . getDeadlineViolationsMaybe

getStFromMaybeDh :: [(HumanTask, Maybe DayHour)] -> [ScheduledTask]
getStFromMaybeDh [] = []
getStFromMaybeDh ((ht,Just x):xs) = (ht,x) : getStFromMaybeDh xs
getStFromMaybeDh ((ht,Nothing):xs) = getStFromMaybeDh xs

scheduleTasks :: WorkCalendar -> FullHours -> [HumanTask]
    -> Maybe [ScheduledTask]
scheduleTasks wcInput fhInput htsInput
    = result
    where
        wc = (nub . sort) wcInput
        fh = (nub . sort) fhInput
        dhTFN = getDayHourToFromNum wc fh
        dhTN = fst dhTFN
        dhFN = snd dhTFN
        hts = sortTasks htsInput
        ets = expandTaskList hts
        stsNum = zip ehts [1..(length ehts)]
            where
                ehts = [ht | (Expanded ht) <- ets]
        sts = [(x, dhFN y) | (x,y) <- stsNum]
        result  | checkDeadlineOrderMaybe sts
                    = Just (getStFromMaybeDh sts)
                | otherwise
                    = Nothing

getStFromMaybeSt :: Maybe [ScheduledTask] -> [ScheduledTask]
getStFromMaybeSt (Just x)   = x
getStFromMaybeSt Nothing    = []

-- fungsi tambahan

getDeadlinesDh :: [HumanTask] -> [Deadline]
getDeadlinesDh hts = [sndTriplet ht | ht <- hts]

getDeadlinesDhn :: WorkCalendar -> FullHours -> [HumanTask]
    -> [Maybe DeadlineNum]
getDeadlinesDhn wcInput fhInput htsInput = result
    where
        wc = (nub . sort) wcInput
        fh = (nub . sort) fhInput
        dhTFN = getDayHourToFromNum wc fh
        dhTN = fst dhTFN
        dhFN = snd dhTFN
        hts = sortTasks htsInput
        result = [(dhTN . sndTriplet) x | x <- hts]

--getDeadlinesDhnRound :: WorkCalendar -> FullHours -> [HumanTask]
--    -> [DeadlineNum]


-- testing

{-
main :: IO()
main = do
    let cal = getDayHourInterval (1,8) (4,11)
            ++ getDayHourInterval (5,8) (5,10)
            ++ getDayHourInterval (1,13) (4,17)
            ++ getDayHourInterval (5,14) (5,17)
    let kls = getDayHourInterval (2,8) (2,10)
            ++ getDayHourInterval (2,13) (2,15)
            ++ getDayHourInterval (3,10) (3,12)
            ++ getDayHourInterval (5,8) (5,10)
            ++ getDayHourInterval (5,13) (5,15)
    let hts = [("tugas teokom", (1,23), 3),
               ("tugas topologi", (2,8), 5),
               ("tugas ranalog", (2,18), 2),
               ("tugas alin2", (5,8), 3),
               ("tugas metnum", (5,13), 4)
              ]
    let sts = scheduleTasks cal kls hts
    print sts
-}

test1Sukses31Okt2024 :: Maybe [ScheduledTask]
test1Sukses31Okt2024 = scheduleTasks cal kls hts
    where
        cal = getDayHourInterval (1,8) (4,11)
            ++ getDayHourInterval (1,13) (4,17)
            ++ getDayHourInterval (5,8) (5,10)
            ++ getDayHourInterval (5,14) (5,17)
        kls = getDayHourInterval (2,8) (2,10)
            ++ getDayHourInterval (2,13) (2,15)
            ++ getDayHourInterval (3,10) (3,12)
            ++ getDayHourInterval (5,8) (5,10)
            ++ getDayHourInterval (5,13) (5,15)
        hts = [("tugas 1", (1,23), 3),
               ("tugas 2", (2,8), 5),
               ("tugas 3", (2,18), 2),
               ("tugas 4", (5,8), 3),
               ("tugas 5", (5,13), 4)
              ]

test2Gagal31Okt2024 :: Maybe [ScheduledTask]
test2Gagal31Okt2024 = scheduleTasks cal kls hts
    where
        cal = getDayHourInterval (1,8) (2,17)
        kls = getDayHourInterval (1,8) (1,11)
            ++ getDayHourInterval (1,13) (1,16)
            ++ getDayHourInterval (2,8) (2,11)
            ++ getDayHourInterval (2,13) (2,16)
        hts = [("tugas 1", (1,23), 3),
               ("tugas 2", (2,8), 5)
               ]

test3Sukses31Okt2024 :: Maybe [ScheduledTask]
test3Sukses31Okt2024 = scheduleTasks cal kls hts
    where
        cal = getDayHourInterval (1,8) (2,23)
        kls = getDayHourInterval (1,8) (1,11)
            ++ getDayHourInterval (1,13) (1,16)
            ++ getDayHourInterval (2,8) (2,11)
            ++ getDayHourInterval (2,13) (2,16)
        hts = [("tugas 1", (1,23), 3),
               ("tugas 2", (2,8), 5)
               ]

{-
main :: IO()
main = do
    print "Test sukses:"
    print (getStFromMaybeSt test1Sukses31Okt2024)
    print "Test gagal:"
    print (getStFromMaybeSt test2Gagal31Okt2024)
    print "Test sukses:"
    print (getStFromMaybeSt test3Sukses31Okt2024)
-}

-- untuk progress 02

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

-- untuk progress 03: algoritma genetika

-- === PART 1: ALGORITMA GNETIKA (GA) UMUM ===

-- placeholder
type Solution = [Int]
type Population = [Solution]

data GAParams = GAParams {
    populationSize :: Int,
    mutationRate   :: Float, -- probabilitas mutasi
    crossoverRate  :: Float, -- probabilitas crossover
    generations    :: Int -- banyaknya iterasi
}

-- nilai fungsi objektif / fitness function
type Fitness = Float

-- fungsi-fungsi untuk masalah yang dihadapi
data GAEnvironment = GAEnvironment {
    fitnessFunc     :: Solution -> Fitness, -- fungsi objektif/fitness

    -- melibatkan Monad karena random:
    crossoverFunc   :: Solution -> Solution -> IO (Solution, Solution),
    mutationFunc    :: Solution -> Float -> IO Solution,
    selectionFunc   :: Population -> IO Solution
}

-- populasi awal random
initializePopulation :: Int -> Int -> IO Population
initializePopulation popSize solutionLength = replicateM popSize (randomSolution solutionLength)

-- solusi awal juga tebakan random
randomSolution :: Int -> IO Solution
randomSolution length = replicateM length (randomRIO (0, 1))

evaluateFitness :: GAEnvironment -> Solution -> Fitness
evaluateFitness env solution = fitnessFunc env solution

selectParent :: GAEnvironment -> Population -> IO Solution
selectParent env population = do
    let k = 3
    candidates <- replicateM k (randomRIO (0, length population - 1))
    return $ maximumBy (compare `on` (evaluateFitness env)) [population !! i | i <- candidates]

crossover :: GAEnvironment -> Solution -> Solution -> IO (Solution, Solution)
crossover env parent1 parent2 = crossoverFunc env parent1 parent2

mutate :: GAEnvironment -> Solution -> IO Solution
mutate env solution = mutationFunc env solution (mutationRate env)

-- satu iterasi GA
nextGeneration :: GAEnvironment -> GAParams -> Population -> IO Population
nextGeneration env params population = do
    newPopulation <- replicateM (populationSize params `div` 2) $ do
        parent1 <- selectParent env population
        parent2 <- selectParent env population
        (child1, child2) <- crossover env parent1 parent2
        child1' <- mutate env child1
        child2' <- mutate env child2
        return [child1', child2']
    return $ concat newPopulation

-- GA full
runGA :: GAEnvironment -> GAParams -> IO Solution
runGA env params = do
    population <- initializePopulation (populationSize params) (length (fitnessFunc env []))
    finalPop <- foldM (\pop _ -> nextGeneration env params pop) population [1..generations params]
    return $ maximumBy (compare `on` (evaluateFitness env)) finalPop

-- === PART 2: HUMAN TASK SCHEDULER DENGAN GA ===

taskSchedulerFitness :: Solution -> Fitness
taskSchedulerFitness schedule = 
    let alpha = 1.0
        beta = 1.0
        gamma = 1.0
        taskDeadlines = []
        completionTimes = []
        dailyWorkloads = []
        adherenceScore = alpha * sum [(d - c) / d | (d, c) <- zip taskDeadlines completionTimes]
        workloadVariance = beta * variance dailyWorkloads
        maxWorkloadPenalty = gamma * maximum dailyWorkloads
    in adherenceScore - workloadVariance - maxWorkloadPenalty
  where
    variance xs = let mean = sum xs / fromIntegral (length xs)
                  in sum [(x - mean) ^ 2 | x <- xs] / fromIntegral (length xs)

taskSchedulerEnv :: GAEnvironment
taskSchedulerEnv = GAEnvironment {
    fitnessFunc = taskSchedulerFitness,
    crossoverFunc = singlePointCrossover,
    mutationFunc = bitFlipMutation,
    selectionFunc = selectParent
}

-- Example GA parameters
gaParams :: GAParams
gaParams = GAParams {
    populationSize = 100,
    mutationRate = 0.01,
    crossoverRate = 0.7,
    generations = 100
}

-- === PART 3: RUNNING ===

main :: IO ()
main = do
    bestSolution <- runGA taskSchedulerEnv gaParams
    putStrLn $ "Solusi terbaik: " ++ show bestSolution
    putStrLn $ "Nilai fitness: " ++ show (taskSchedulerFitness bestSolution)
