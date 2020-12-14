data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read) 
newtype CountryCode = MakeCountryCode Integer deriving (Eq)
newtype PhoneNo = MakePhoneNo Integer deriving (Eq)

instance Show CountryCode where
  show (MakeCountryCode n) = id ("+" ++ show n)
instance Show PhoneNo where
  show (MakePhoneNo n) = show n

createCC :: Integer -> CountryCode
createCC n 
  | n < 0 = error "Negative CountryCode!"
  | otherwise = MakeCountryCode n

createPN :: Integer -> PhoneNo
createPN n 
  | n < 0 = error "Negative PhoneNo!"
  | otherwise = MakePhoneNo n
  
data Phone = MakePhone {phoneType :: Maybe PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo} deriving (Eq)

--instance Show Phone where
  --show (MakePhone Nothing Nothing pn) = show pn
--  show (MakePhone (Just a) (Just b) c) = show b ++ " " ++ show c ++ " (" ++ show a ++ ")"

createPhone :: Maybe PhoneType -> Maybe CountryCode -> PhoneNo -> Phone
createPhone pt cc pn = MakePhone pt cc pn

countryCodes = [358, 666, 777, 123]

inList :: String -> String
inList s = if elem (read s) countryCodes then s else error "CountryCode is not in the list"

checkCC :: String -> String
checkCC s
  | head s == '+' = inList $ tail s
  | take 2 s == "00" = inList $ drop 2 s
  | otherwise = inList $ s 

--readPhone :: String -> String -> String -> Phone
--readPhone "" s2 s3 = createPhone Nothing (createCC $ read $ checkCC s2 ) (createPN $ read s3)
--readPhone s1 s2 s3 = createPhone (read s1) (createCC $ read $ checkCC s2 ) (createPN $ read s3)