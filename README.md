# Платформа EdgeNode
 > [описание языка разметки markdown](https://www.markdownguide.org/) 

 Описание элементов и их связей в контексте серверной части 
 Удаление в edgeNode является логическим, т.е.запись сохранятся в бд, но помечается как удаленная.

 enum типы:
 `data QuaififcationDegree = 
         BSc
       | BA
       | MSc
       | MA
       | MRes
       | Bakalavr
       | Magistr
       | AdvancedLevelGCE
       | UnifiedStateExam` (equates to type)
  `data StudyTime = PartTime | FullTime | Sandwich`
  `data AcademicArea = 
          Mathematics
        | Physics
        | History
        | Economics
        | Finance
        | Biology
        | Physiology
        | FineArts`
  `data Per = Annum | Month | Day`
  `data Country = Russia | UnitedKingdom`
  `data Language = Ru | En`
  `data Gender = Male | Female`
  `data Currency = RUB | USD | EUR`
  `data QuaififcationCategory =
          StateExam 
        | HigherDegree 
        | LanguageStandard 
        | InternationalDiploma` 
   `data RegistrationStatus = Active | Wait | TimeOut | Banned`
   `data TemplateSource = Degree | Qualification`
   `data AuthUserType = User | Provider` 
    
 1. **Cистема аутентификации**
    представлена двумя типами (формами в рамках front) для первичного (user) и вторичного (provider) пользователя.
    первичный пользователь: email (уникальный в edgeNode, пароль).
    вторичный пользователь: provider uid в edgeNode, login уникальный в рамках provider, пароль.
    Аутентификации проходит в рамках таблицы auth.user:
     - перввичный пользователь: identifier ~ email
     - вторичный пользовталь: identifier ~ providerId#login

    После аутентификации клиент получает информацию кто 
    он для правильного отображения на стороне front
    WhoAmI: AuthUserType (enum type (haskell)).

 2. **Auth**
    [в основу положен черновик rbac модели](https://csrc.nist.gov/CSRC/media/Projects/Role-Based-Access-Control/documents/sandhu96.pdf)
    корневая роль: root,
    первичный пользователь: user
    вторичный пользователь: provider, guest 

    UserId -> auth.user(id)

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

     *verifyAuthorization
     
    Permission - операции доступные для данной роли 
    Controller - в случае успеха проваливаемся сюда (controller)
    401 - в случае если права отсутствуют или не достаточны
    таблицы:
  >  
     auth.role 
       id: int8, serial
       title: text not null
       description: text, nullable
       parent_fk: int8 null refer to auth.role (id)
       created: time, not null
       modified: time, nullable       
       unique: title 
       
     auth.permission 
       id: int8, serial
       title: text not null
       description: text, nullable
       parent_fk: int8 null refer to auth.permission (id)
       created: time, not null
       modified: time, nullable  
       unique: title

     auth.user_role
       user_fk: int8, not null, refer to auth.user (id)
       role_fk: int8, not null, refer to auth.role (id)
       unique: (user_fk, role_fk)

     auth.role_permission
       permission_fk: int8, not null, refer to auth.permission (id)
       role_fk: int8, not null, refer to auth.role (id)
       unique: (user_fk, role_fk)

     auth.user 
       id: int8, serial
       identifier: text, not null
       password: bytea, not null
       created: time, not null
       modified: time, nullable
       user_type: text not null, -> enum type (haskell) 
       index: identifier     
       unique: identifier

     auth.token
       id: int8, serial
       token: bytea not null
       user_fk: int8, not null, refer to auth.user (id)
       created, time not null
       unique: text, not null
       
 3. **Первичный пользователь (user)**
  таблицы:
  >
    edgenode.user
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
      user_fk: int8, not null, refer to auth.user
      unique: user_fk

 4. **Вторичный пользователь (далее provider)** 
   таблицы:
   >  
      edgenode.provider
        id: int8, serial
        uid: text, not null 
        created: time, not null
        unique: uid

      edgenode.provider_user
        id: int8, serial
        email: text, not null
        status: text, not null, -> enum type (haskell) 
        provider_fk: int8, not null, refer to edgenode.provider
        user_fk: int8, not null, refer to auth.user
        created: time, not null
        modified: time, nullable
        unique: (provider_fk, user_fk)

      edgenode.provider_branch
        id: int8, serial 
        title: text, not null
        country: text, not null -> enum type (haskell)
        address: text, not null
        is_hq: bool, not null. default false
        created: time, not null
        modified: time, nullable
        deleted: time, nullable
        is_deleted: bool, not null, default false
        provider_fk: int8, not null, refer to edgenode.provider

      edgenode.provider_branch_qualification    
        id: int8, serial
        title: text, not null
        type: text, not null -> enum type (haskell)
        study_time: text, not null -> enum type (haskell)
        academic_area: list of text, -> enum type (haskell)
        start: date, not null
        end: date, not null
        is_repeated: bool, not null, default false
        application_deadline: date, not null
        category: text, not null -> enum type (haskell)
        created: time, not null
        modified: time, nullable
        deleted: time, nullable
        is_deleted: bool, not null, default false
        provider_branch_fk: int8, not null, refer to edgenode.provider_branch

      edgenode.provider_branch_qualification_tuition_fees
        id: int8, serial
        amount: int8, not null
        currency: text, not null, -> enum type (haskell)
        per: text, not null, -> enum type (haskell)
        country: text, not null -> enum type (haskell)
        provider_qualification_fk: int8, not null, refer to edgenode.provider_qualification

      https://blog.typeable.io/posts/2019-11-21-sql-sum-types.html
      create type unit as enum ('()');
      edgenode.provider_branch_qualification_dependency
        id: int8, serial
        provider_branch_qualification_fk: int8, not null, refer to edgenode.provider_branch_qualification 
        created: time, not null
        modified: time, nullable
        deleted: time, nullable
        is_deleted: bool, not null, default false
        degree unit,
        qualification unit,
        check((degree is not null and qualification is null) or 
              (degree is null and qualification is not null))
        constraint provider_branch_qualification_dependency__degree foreign key (id, degree) references degree (id, tag),
        constraint provider_branch_qualification_dependency__qualification foreign key (id, Qualification) references qualification (id, tag),

      edgenode.degree
        id: int8, not null
        tag: unit not null default '()',
        type: text, not null -> enum type (haskell)
        grade: jsonb, nullable (value)
        primary key (id,tag)

      edgenode.qualification
        id: int8, not null
        tag: unit not null default '()',
        provider_branch_qualification_fk: int8, not null, refer to edgenode.provider_branch_qualification
        grade: jsonb, not null (value)
        primary key (id,tag)

      edgenode.qualification_type_country
        id: int8, serial
        country: list of text, not null -> enum type (haskell)
        qualififcation_type: text, not null, -> enum type (haskell)
        unique: qualififcation_type

      edgenode.qualification_type_grade
        id: int8, serial
        qualififcation_type: text, not null, -> enum type (haskell)
        grade: range, type (text, int)
        unique: qualififcation_type

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
      source: text, not null -> enum type (haskell)
      template: text, not null