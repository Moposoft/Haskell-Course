data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)  

type PhoneNo = Integer
type CountryCode = Integer

data Phone = Phone { phoneType :: PhoneType, countryCode :: CountryCode, phoneNo :: PhoneNo } deriving (Show, Eq, Read)

createPhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
createPhone x y z 
  | y < 0 && z < 0 = error "Negative CountryCode and negative PhoneNo!"
  | y < 0 = error "Negative CountryCode!"
  | z < 0 = error "Negative PhoneNo!"
  | otherwise = Phone x y z