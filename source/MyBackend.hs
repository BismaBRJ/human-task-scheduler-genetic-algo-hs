module MyBackend where
    import Data.List (sort, sortBy, nub, maximumBy)
    import Data.Function (on)

    type HourNum = Int
    type DayNum = Int
    type DayHour = (DayNum, HourNum)

    type WorkCalendar = [DayHour]
    type FullHours = [DayHour]
    type DayHourNum = Int

    type TaskName = String
    type Deadline = DayHour
    type DeadlineNum = DayHourNum
    type Duration = Int
    type HumanTask = (TaskName, Deadline, Duration)

    tempDayHours :: [DayHour]
    tempDayHours = [(0,0)]
    
    tempHumanTasks :: [HumanTask]
    tempHumanTasks = [("Untitled", (0,0), 0)]

    fstTriplet :: (a,b,c) -> a
    fstTriplet (x,y,z) = x
    sndTriplet :: (a,b,c) -> b
    sndTriplet (x,y,z) = y
    trdTriplet :: (a, b, c) -> c
    trdTriplet (x,y,z) = z

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

    getDayHourInterval :: DayHour -> DayHour -> WorkCalendar
    getDayHourInterval p1 p2
        = [(x,y) | x <- [(fst p1)..(fst p2)], y <- [(snd p1)..(snd p2)]]

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

    -- TESTS

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
