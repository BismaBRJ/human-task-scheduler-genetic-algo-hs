-- import

import Data.List (sort, sortBy, nub)

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

getHtTn :: HumanTask -> TaskName
getHtTn (tn,dl,dur) = tn

getHtDl :: HumanTask -> Deadline
getHtDl (tn,dl,dur) = dl

getHtDur :: HumanTask -> Duration
getHtDur (tn,dl,dur) = dur

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
    = concat [replicate (getHtDur ht) (Expanded ht) | ht <- hts]

getDeadlineViolations :: [ScheduledTask] -> [ScheduledTask]
getDeadlineViolations stsInput = reverse backwardsResult
    where
        backwardsSts = (nub . reverse) stsInput
        backwardsResult
            = [ st | st <- backwardsSts,
                getHtDl (fst st) < snd st ]

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
                getHtDl (fst st) < dhJustOrZero (snd st) ]

checkDeadlineOrder :: [(HumanTask, Maybe DayHour)] -> Bool
checkDeadlineOrder = null . getDeadlineViolationsMaybe

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

test31Okt2024 :: Maybe [ScheduledTask]
test31Okt2024 = scheduleTasks cal kls hts
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

main :: IO()
main = do
    print (getStFromMaybeSt test31Okt2024)
