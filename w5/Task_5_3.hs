import qualified Data.Map.Strict as Map

-- 4.2
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read) 
data CountryCode = CountryCode Integer deriving (Eq)
data PhoneNo = PhoneNo Integer deriving (Eq)

instance Show CountryCode where
  show (CountryCode n) = id ("+" ++ show n)
instance Show PhoneNo where
  show (PhoneNo n) = show n

createCC :: Integer -> CountryCode
createCC n 
  | n < 0 = error "Negative CountryCode!"
  | otherwise = CountryCode n

createPN :: Integer -> PhoneNo
createPN n 
  | n < 0 = error "Negative PhoneNo!"
  | otherwise = PhoneNo n
  
data Phone = Phone {phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo} deriving (Eq)

instance Show Phone where
  show (Phone a b c) = show b ++ " " ++ show c ++ " (" ++ show a ++ ")"

createPhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
createPhone pt cc pn = Phone pt cc pn

-- 4.3 
countryCodes = [358, 666, 777, 123]

inList :: String -> String
inList s = if elem (read s) countryCodes then s else error "CountryCode is not in the list"

checkCC :: String -> String
checkCC s
  | head s == '+' = inList $ tail s
  | take 2 s == "00" = inList $ drop 2 s
  | otherwise = inList $ s 

readPhone :: String -> String -> String -> Phone
readPhone s1 s2 s3 = createPhone (read s1) (createCC $ read $ checkCC s2 ) (createPN $ read s3)


-- 5.3
type Name = String
type Phonebook = Map.Map Name [Phone]
myPhoneBook = Map.fromList 
  [("PersonA", [readPhone "WorkLandline" "00358" "123456789"]),
  ("PersonB", readPhone "WorkLandline" "358" "2323"),
  ("PersonC", readPhone "WorkLandline" "358" "12312123")]

--findEntries :: String -> Map.Map -> Maybe a
findEntries name pb = Map.lookup name pb

-- insertWith (++) (read name) phone (fromList PhoneBook)
--addEntry :: Name -> String -> String -> String -> Phonebook -> PhoneBook