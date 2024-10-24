-- AP: After Present
-- relatif terhadap masa kini
data HumanTask = HumanTask {
    taskName :: String,
    deadlineAP :: Integer
    -- deadlineAP berupa beberapa satuan waktu (bulat),
    -- relatif terhadap masa kini;
    -- yaitu, beberapa satuan waktu di masa depan
} deriving (Show)
-- type HumanTasks = [HumanTask]
