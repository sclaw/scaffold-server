# Платформа EdgeNode
 > [описание языка разметки markdown](https://www.markdownguide.org/) 

 Описание элементов и их связей в контексте серверной части 
 Удаление в edgeNode является логическим, т.е.запись сохранятся в бд, но помечается как удаленная.
 1. **Cистема аутентификации**
    предсталена двумя типами (формами в рамках front) для первичного (user) и вторичного (provider) пользователя.
    первичный пользователь: email (уникальный в edgeNode, пароль).
    вторичный пользователь: provider uid в edgeNode, login уникальный в рамках provider, пароль.
    После аутентификации клиент получает информацию кто 
    он для правильного отображения на стороне front
    WhoAmI: user, provider (enum type (haskell)).

 2. **Cистема разграничения доступа**
    [в основу положен черновик rbac модели](https://csrc.nist.gov/CSRC/media/Projects/Role-Based-Access-Control/documents/sandhu96.pdf)
    корневая роль: root,
    первичный пользователь: user
    вторичный пользователь: provider, guest  
    схема:
    роли
    >
                  root
          _________|_________
          |                  |
         user             provider
                             | 
                           guest
    разрешения
    >
                      root
             _________ |_________ 
             |                   |
            user            providerAdmin
                                 |
                            providerGuest        

    авторизация: 
     > **verifyAuthorization :: UserId -> Permission -> Controller -> Controller**

    Permission - операции доступные для данной роли 
    Controller - в случае успеха проваливаемся сюда (controller)
    401 - в случае если права отсутствуют или не достаточны
    таблицы:
  >  
     edgeNode.Role 
       id: int8, serial
       title: text not null
       parent: int8 null refer to edgeNode.Role (id)
       unique: title 
       
     edgeNode.Permission 
       id: int8, serial
       title: text not null
       parent: int8 null refer to edgeNode.Permission (id)
       unique: title

     edgeNode.UserRole
       userFK: int8, not null, refer to edgeNode.User (id)
       roleFK: int8, not null, refer to edgeNode.ROle (id)
       unique: (userFK, roleFK)

 3. **Первичный пользователь (user)**
  таблицы:
  >
    auth.User 
      id: int8, serial
      email: text, not null
      password: bytea, not null
      created: time, not null
      modified: time, nullable
      index: email     
      unique: email

    edgeNode.User
      id: int8, serial
      name: text, nullable
      surname: text, nullable
      middlename: text, nullable
      birth: json or bytea 
      allegiance: text, not null (provided by edgeNode)
      avatar: bytea (move to aws)
      gender: text, not null, -> enum type (haskell)
      status: text, not null, -> enum type (haskell)
      created: time, not null
      modified: time, nullable
      userFK: int8, not null, refer to auth.User
      unique: userFK

 4. **Вторичный пользователь (далее provider)** 
   таблицы:
   >  
      edgeNode.Provider
        id: int8, serial
        uid: text, not null 
        created: time, not null
        unique: uid

      edgeNode.ProviderUser
        id: int8, serial
        login: text, not null
        email: text, not null
        status: text, not null, -> enum type (haskell) 
        providerFK: int8, not null, refer to edgeNode.Provider
        providerUserFK: int8, not null, refer to auth.ProviderUser
        created: time, not null
        modified: time, nullable
        unique: (login, providerFK)

      auth.ProviderUser
        id: int8, serial
        pass: text, not null
        created: time, not null
        modified: time, nullable

      edgeNode.ProviderBranch
        id: int8, serial 
        title: text, not null
        country: text, not null -> enum type (haskell)
        address: text, not null
        isHQ: bool, not null. default false
        created: time, not null
        modified: time, nullable
        deleted: time, nullable
        isDeleted: bool, not null, default false
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
        category: text, not null -> enum type (haskell)
        created: time, not null
        modified: time, nullable
        deleted: time, nullable
        isDeleted: bool, not null, default false
        providerBranchFK: int8, not null, refer to edgeNode.ProviderBranch

      edgeNode.ProviderBranchQualificationTuitioFees
        id: int8, serial
        amount: int8, not null
        currency: text, not null, -> enum type (haskell)
        per: text, not null, -> enum type (haskell)
        country: text, not null -> enum type (haskell)
        providerQualificationFK: int8, not null, refer to edgeNode.ProviderQualification

      https://blog.typeable.io/posts/2019-11-21-sql-sum-types.html
      create type unit as enum ('()');
      edgeNode.ProviderBranchQualificationDependency
        id: int8, serial
        providerBranchQualificationFK: int8, not null, refer to edgeNode.ProviderBranchQualification 
        created: time, not null
        modified: time, nullable
        deleted: time, nullable
        isDeleted: bool, not null, default false
        Degree unit,
        Qualification,
        check((Degree is not null and Qualification is null) or 
              (Degree is null and Qualification is not null))
        constraint providerBranchQualificationDependency__degree foreign key (id, Degree) references Degree (id,tag),
        constraint providerBranchQualificationDependency__qualification foreign key (id, Qualification) references Qualification (id, tag),

      edgeNode.Degree
        id: int8, not null
        tag: unit not null default '()',
        type: text, not null -> enum type (haskell)
        grade: jsonb, nullable (value)
        primary key (id,tag)

      edgeNode.Qualification
        id: int8, not null
        tag: unit not null default '()',
        providerBranchQualificationFK: int8, not null, refer to edgeNode.ProviderBranchQualification
        grade: jsonb, not null (value)
        primary key (id,tag)

      edgeNode.QualificationTypeCountry
        id: int8, serial
        country: list of text, not null -> enum type (haskell)
        qualififcationType: text, not null, -> enum type (haskell)
        unique: qualififcationType

      edgeNode.QualificationTypeGrade
        id: int8, serial
        qualififcationType: text, not null, -> enum type (haskell)
        grade: range, type (text, int)
        unique: qualififcationType

 5. **Регистрация (user)** 
    для регистрации вводится email, пароль.
    До потверждения email, которое должно прийти на зарегистрированный ранее email, пользователь находится в статусе 'ждет подтверждения'.
    После: переходит в статус подтвержден. Временной лимит для подтверждения: 1 неделя (переменная величина). 
    > Status: approved, timeRunOut, wait - enum type (haskell)

 6. **Регистрация (provider)** 
    региcтрация будет осуществлятся силами edgenode.
    после успешной регистрации провайдеру присваивается уникальный идентефикатор (uid), создается user admin и пароль (pass)
    > uid: ru1#0000x

    состоит из страны, типа провайдера для данной страны, порядковый номер в системе edgenode среди равных провайдеров по типу
    ручки: 
    1 для отображение стран и типов 
    
    > GET amdin/registration/country, resp: list of countries (text), permission: root
      GET admin/registration/types, resp: list of types (text, full), permission: root

    нужна таблица соответствия между страной и типами:
    > edgeNode.ProviderType
       id: int8, serial
       country: text, not null
       typeConcise: text, not null
       typeFull: text, not null 

    2 регистрация 
    > PUT admin/registration, permission: root

    послe отправки формируется uid, admin и pass, 
    admin присваивается роль providerAdmin

 7. **Личный кабинет, отделения (provider)**
  необходимый уровень прав: provider, op: rw
                            guest, op: r 
  ручки:
      > PUT provider/profile/branch, permission: providerAdmin
        GET provider/profile/branch, permission: providerGuest 
        PATCH provider/profile/branch, permission: providerAdmin
        DELETE provider/profile/subsidiary - только дочернии отделения, permission: providerAdmin
        PUT provider/profile/branch/hq/assign - назначить головное отделение, permission: providerAdmin

    каждый провайдер представлен своими отделениями: главным и дочерними

 8. **Личный кабинет, квалификация (provider)**
    необходимый уровень прав: role: provider, op: rw
    состоит из 2 частей: сама квалификация (edgeNode.ProviderBranchQualification, edgeNode.ProviderBranchQualificationTuitioFees),
    рекомендуемые требования (edgeNode.ProviderBranchQualificationDependency)
    тип квалификации определяется тем в какой стране находится отделение

 9. **Личный кабинет, настройки (provider)** 
   - добавление пользователя
     Администратор отправляет в edgeNode запрос на добавление нового пользователя.
     req: login, email - куда будет отправлен временный пароль
     > PUT provider/settings/user, permission: providerAdmin
       PUT provider/settings/user/password/resend, permission: providerAdmin

   - Получение всех пользователей
     > GET provider/settings/user, permission: providerAdmin    

   - удаление пользователей
     Администатор может удалять пользователей
     > DELETE provider/settings/user, permission: providerAdmin

   - смена пароля
     Данные: старый пароль, новый пароль, подтверждение нового пароля.
     > PUT provider/settings/password/change, permission: providerGuest

 10. **квалификация (provider)**
     Добавление:
      .......
    Удаление:
      .......
     ручки: 
     >  GET provider/qualififcation/{qid}
        PUT provider/qualififcation
        PATCH provider/qualififcation/{qid}
        DELETE provider/qualififcation/{qid}

 11. **файлы**
     ........
   
 12. **правила при добавлении требований для квалификации**
     - каждому типу в квалификации (type) сопоставляется набор из академичесикх областей (academic area): type -> [academic area]
     - каждому типу (type) сопоставляется квалификация.
  
 13. **шаблоны**
     таблица:
  > 
    edgeNode.Template
      id: int8, serial
      type: text, not null -> enum type (haskell)
      template: text, not null