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
createPhone x y z = Phone x y z