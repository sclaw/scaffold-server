# edgeNode-server

# вторичный пользователь (далее provider)
 регистрация 
 личный кабинет
 таблицы:
   edgeNode.Provider
     id: int8, serial
     uid: text, not null
   auth.Provider
    id: int8, serial
     pass: text, not null
     providerFK: int8, not null, refer to edgeNode.Provider
   edgeNode.ProviderBranch
    id: int8, serial 
    title: text, not null
    country: text, not null -> enum type (haskell)
    address: text, not null
    isHQ: bool, not null. default false
    providerFK: int8, not null, refer to edgeNode.Provider
   edgeNode.ProviderBranchQualification    
    id: int8, serial
    title: text, not null
    type: text, not null -> enum type (haskell)
    studyTime: text, not null -> enum type (haskell)
    academicArea: list of text, -> enum type (haskell)
    start: date, not null
    end: date, not null
    isRepeated: bool, not null, default false
    applicationDeadline: date, not null
   edgeNode.ProviderBranchQualificationTuitioFees
    id: int8, serial
    amount: int8, not null
    currency: text, not null, -> enum type (haskell)
    per: text, not null, -> enum type (haskell)
    country: text, not null -> enum type (haskell)
    providerQualificationFK: int8, not null, refer to edgeNode.ProviderQualification
   edgeNode.ProviderBranchQualificationDependency
    id: int8, serial   
 
# регистрация (provider) 
региcтрация будет осуществлятся силами edgenode.
после успешной регистрации провайдеру присваивается уникальный идентефикатор (uid), пароль (pass)
uid: ru1#0000x
 состоит из страны, типа провайдера для данной страны, порядковый номер в системе edgenode среди равных провайдеров по типу

ручки: 
 1 для отображение стран и типов 
   GET provider/registration/country, resp: list of countries (text)
   GET provider/registration/types, resp: list of types (text, full)
   нужна таблица соответствия между страной и типами:
    edgeNode.ProviderType
     id: int8, serial
     country: text, not null
     typeConcise: text, not null
     typeFull: text, not null 

 2 регистрация 
   PUT provider/registration, body: country, type

послe отправки формируется uid и pass

# личный кабинет, отделения (provider)
ручкм:
PUT provider/profile/branch
GET provider/profile/branch 
PATCH provider/profile/branch
DELETE provider/profile/subsidiary - только дочернии отделения.
PUT provider/profile/branch/hq/assign - назначить головное отделение

каждый провайдер представлен своими отделениями: главным и дочерними

# личный кабинет, квалификация (provider)
состоит из 2 частей: сама квалификация (edgeNode.ProviderBranchQualification, edgeNode.ProviderBranchQualificationTuitioFees),
реккомендуемый требования (edgeNode.ProviderBranchQualificationDependency)



