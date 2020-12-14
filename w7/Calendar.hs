import Data.List

split :: Char -> String -> [String]
split c =
  map (takeWhile (/= c)) .
  takeWhile (not . null) .
  iterate (dropWhile (==  ' ') . drop 1 . dropWhile (/= c))
  
readInput :: String -> [String]
readInput arg = 
  let s = split '\'' arg
  in s

readDate :: String -> Date
readDate arg = 
  let s = split '-' arg 
      y = read (s !! 0) :: Integer
      m = read (s !! 1) :: Integer
      d = read (s !! 2) :: Integer
  in makeDate y m d

getYear :: String -> Integer
getYear arg = 
  let s = split '-' arg
      y = read (s !! 0) :: Integer
  in y

getMonth :: String -> Integer
getMonth arg = 
  let s = split '-' arg
      m = read (s !! 1) :: Integer
  in m

getDay :: String -> Integer
getDay arg = 
  let s = split '-' arg
      d = read (s !! 2) :: Integer
  in d

printEventDay :: EventInfo -> String
printEventDay e =
  let n = name e
      d = show $ date e
      s = ("Event " ++ n ++ " happens on " ++ d ++ "\n")
  in s

printEventPlace :: EventInfo -> String
printEventPlace e =
  let n = name e
      p = place e
      s = ("Event " ++ n ++ " happens at " ++ p ++ "\n")
  in s

doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
  events <- ioEvents
  let args = readInput input
      date1 = readDate (args !! 5)
      name1 = (args !! 1)
      place1 = (args !! 3)
      event = EventInfo name1 place1 date1
      eventname = (args !! 1) 
      findEvent = filter (\x -> name x == eventname) events
      getDate = readDate (args !! 1)
      findEventDate = filter (\x -> date x == getDate) events
      getPlace = (args !! 1)
      findEventPlace = filter (\x -> place x == getPlace) events
      a = (args !! 5)
  if length args == 6 && (args !! 0) == "Event " && (args !! 2) == "happens at " && (args !! 4) == "on "
  then if correctDate (getYear a) (getMonth a) (getDay a) then do
      let new = filter (\x -> name x /= name1) events
      putStrLn "ok"
      loop $ return (event:new)
    else do 
      putStrLn "Bad date"
      loop $ return events
  else if length args == 2 && (args !! 0) == "Tell me about "
  then if (length $ findEvent) > 0 then do
      let pn = place (head findEvent)
          en = name (head findEvent)
          d = show $ date (head findEvent) 
      putStrLn ("Event " ++ en ++ " happens at " ++ pn ++ " on " ++ d)
      loop $ return events 
  else do
      putStrLn "I do not know of such event"
      loop $ return events
  else if length args == 2 && (args !! 0) == "What happens on "
  then if (length $ findEventDate) > 0 then do
      let out = sort $ map printEventDay findEventDate
      putStrLn $ init $ concat out 
      loop $ return events 
    else do   
      putStrLn "Nothing that I know of"
      loop $ return events
  else if length args == 2 && (args !! 0) == "What happens at "
  then if (length $ findEventPlace) > 0 then do
      let out = sort $ map printEventPlace findEventPlace
      putStrLn $ init $ concat out
      loop $ return events 
    else do   
      putStrLn "Nothing that I know of"
      loop $ return events
  else do
    putStrLn "I do not understand that. I understand the following:" 
    putStrLn "*Event <name> happens at <place> on <date>"
    putStrLn "*Tell me about <eventname>" 
    putStrLn "*What happens on <date>"
    putStrLn "*What happens at <place>" 
    putStrLn "*Quit"
    loop $ return events
 
-- date?

-- Note: this is not a complete implementation for dates.
-- This is just a data type example.

-- The naive approach: (I use ' in the names not to confuse with stuff later
-- This is really no safer than (Int,Int,Int) or (Integer, Integer, Integer)
-- To change the types you only go to one place (the type definition)
-- but nothing stops you from mixing up days, months and years
-- between themselves and other Int-typed variables
-- Load this file and try the following (without --): 
-- Date' (2 :: Month') (2 :: Month') (2 :: Month')


type Year' = Integer
type Month' = Integer
type Day' = Integer

data Date' = Date' { year' :: Year', month' :: Month', day' :: Day' } deriving (Eq, Show, Ord)

-- "data" wraps data inside a wrapper that makes it different
-- from other types:

data Month = MakeMonth Integer deriving (Eq, Ord)

instance Show Month where 
  show (MakeMonth x) = if x < 10 then '0' : show x
  else show x

-- If all values are ok, it is enough to call "MakeMonth x"
-- Now the input is an Integer. What if it was a String?
-- - then MakeMonth (read x :: Integer) would do it
toMonth               :: Integer -> Month
toMonth x
  | x < 1     = error "Minimum month number is 1" 
  | x > 12     = error "Maximum month number is 12" 
  | otherwise = MakeMonth x

fromMonth             :: Month -> Integer
fromMonth (MakeMonth i) = i  -- Pattern match i out 

-- This is done similarly as Month
data Day = MakeDay Integer deriving (Eq, Ord)

instance Show Day where 
  show (MakeDay x) = if x < 10 then '0' : show x
  else show x

toDay               :: Integer -> Day
toDay x
  | x < 1     = error "Minimum day number is 1" 
  | x > 31     = error "Maximum day number is 31" 
  | otherwise = MakeDay x

fromDay             :: Day -> Integer
fromDay (MakeDay i) = i 

{- this would take care of year 0. I have commented this out
   since I will use "newtype" to demonstrate it.

data Year = MakeYear Integer deriving (Eq, Show)

toYear               :: Integer -> Year
toYear x
  | x == 0     = error "No year 0" 
  | otherwise = MakeYear x

fromYear             :: Year -> Integer
fromYear (MakeYear i) = i  

instance Num Year where
    fromInteger         = toYear
    x + y               = toYear $ fromYear x + fromYear y 
    x - y               = toYear $ fromYear x - fromYear y 
    x * y               = toYear $ fromYear x * fromYear y

-}


-- This does not take care of year 0 ie it allows it
-- But this is straightforward

newtype Year = MakeYear Integer deriving (Eq, Show, Ord)

toYear :: Integer -> Year
toYear x
 | x == 0 = error "No year 0"
 | otherwise = MakeYear x

fromYear :: Year -> Integer
fromYear (MakeYear x) = x

-- A record type. Notice that we get functions year, month, and day
-- that are used in the functions below. The record field names
-- year, month and day are also used in assignign values - see below.

data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Eq, Ord)

instance Show Date where
  show date = (show $ fromYear (year date)) ++ "-" ++ (show (month date)) ++ "-" ++ (show (day date))

-- Examples of applications of the types:

-- A function to check if a year is a leap year

leapYear (MakeYear y)
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False


makeMaybeDate :: Integer -> Integer -> Integer -> Maybe Date
makeMaybeDate y m d
 | y == 0 = Nothing
 | elem m [1,3,5,7,8,10,12] &&
   elem d [1..31] = makeJustDate y m d
 | elem m [4,6,9,11] &&
   (elem d [1..30]) = makeJustDate y m d
 | m==2 && elem d [1..28] = makeJustDate y m d
 | leapYear (toYear y) && m==2 && d==29 = makeJustDate y m d
 | otherwise = Nothing
 where makeJustDate y m d = Just Date {year = toYear y, month = toMonth m, day = toDay d}



-- 3: Write a function to check if a given date (y,m,d)
--    is correct

correctDate :: Integer -> Integer -> Integer -> Bool
correctDate 0 _ _  = False
correctDate y m d
 | (elem m [1,3,5,7,8,10,12]) && (elem d [1..31]) = True
 | (elem m [4,6,9,11]) && (elem d [1..30]) = True
 | (m==2) && (elem d [1..28]) = True
 | (leapYear (toYear y)) && (m==2) && (d==29) = True
 | otherwise = False


makeDate :: Integer -> Integer -> Integer -> Date
makeDate y m d
 | correctDate y m d = Date { year = toYear y, month = toMonth m, day = toDay d }
 | otherwise = error "not correct combination of integers for year, month and day"


-- 4: Write a function that, given a date,
--    calculates the next date

nextDate :: Date -> Date
nextDate date
  | correctDate y m (d+1) =  Date { year = year date, month = month date, day = toDay (d+1) }
  | correctDate y (m+1) 1 = Date { year = year date, month = toMonth (m+1), day = toDay 1  }
  | y == (-1) = Date { year = toYear 1, month = toMonth 1, day = toDay 1 }
  | otherwise = Date { year = toYear (y+1), month = toMonth 1, day = toDay 1 }
  where y = fromYear $ year date
        m = fromMonth $ month date
        d = fromDay $ day date


-- 5: distance of two dates

dateDistance :: Date -> Date -> Integer
dateDistance date1 date2
  | not (correctDate y1 m1 d1) = error "only calculate distance for correct dates"
  | not (correctDate y2 m2 d2) = error "only calculate distance for correct dates"
  | date1 == date2 = 0
  | date1 < date2 = addDateUntil' date1 date2 0 -- tail recursion
  | otherwise = addDateUntil date2 date1 -- not tail recursion
  where y1 = fromYear $ year date1
        m1 = fromMonth $ month date1
        d1 = fromDay $ day date1
        y2 = fromYear $ year date2
        m2 = fromMonth $ month date2
        d2 = fromDay $ day date2


-- This would not use tail recursion
addDateUntil date1 date2 
  | date1 == date2 = 0
  | otherwise = 1 + addDateUntil (nextDate date1) date2

-- this one uses tail recursion
addDateUntil' date1 date2 n
  | date1 == date2 = n
  | otherwise = addDateUntil' (nextDate date1) date2 (n+1)
 


-- This is getting out of the course contents, but out of interest:
-- The following makes Month to be a number in the Num typeclass
-- If no calculations are needed, this is not needed.
-- I do not define abs or signum. It seems to go through.
-- I only allow positive values so they are not relevant.

instance Num Month where
    fromInteger         = toMonth
    x + y               = let r = fromMonth x + fromMonth y in
                            if r < 1 || r > 12 then error "Unnatural addition for month"
                                     else toMonth r
    x - y               = let r = fromMonth x - fromMonth y in
                            if r < 1 || r > 12 then error "Unnatural subtraction for month"
                                     else toMonth r
    x * y               = let r = fromMonth x * fromMonth y in
                            if r < 1 || r > 12 then error "Unnatural multiplication for month"
                                     else toMonth r

instance Num Day where
    fromInteger         = toDay
    x + y               = toDay $ fromDay x + fromDay y
    x - y               = toDay $ fromDay x - fromDay y
    x * y               = toDay $ fromDay x * fromDay y


 