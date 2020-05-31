{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module EdgeNode.Controller.Controller (controller) where

import EdgeNode.Api
import EdgeNode.Transport.Response
import qualified EdgeNode.Model.Rbac as Rbac
import qualified EdgeNode.Statement.Rbac as Rbac
-- controllers
import qualified EdgeNode.Controller.File.Upload as File.Upload
import qualified EdgeNode.Controller.File.Download as File.Download
import qualified EdgeNode.Controller.File.Delete as File.Delete
import qualified EdgeNode.Controller.File.Patch as File.Patch
import qualified EdgeNode.Controller.Admin.ProviderRegister as Admin.ProviderRegister
import qualified EdgeNode.Controller.Admin.ResetPassword as Admin.ResetPassword
import qualified EdgeNode.Controller.Provider.GetBranches as Provider.GetBranches
import qualified EdgeNode.Controller.Provider.CreateBranches as Provider.CreateBranches
import qualified EdgeNode.Controller.Provider.PatchBranch as Provider.PatchBranch
import qualified EdgeNode.Controller.Provider.DeleteBranch as Provider.DeleteBranch
import qualified EdgeNode.Controller.Provider.SetHQ as Provider.SetHQ
import qualified EdgeNode.Controller.Search.GetBarItems as Search.GetBarItems
import qualified EdgeNode.Controller.Search.GetQualificationList as Search.GetQualificationList
import qualified EdgeNode.Controller.Search.GetQualificationModal as Search.GetQualificationModal
import qualified EdgeNode.Controller.Auth.SignIn as Auth.SignIn
import qualified EdgeNode.Controller.Auth.Registration as Auth.Registration
import qualified EdgeNode.Controller.Auth.SignOut as Auth.SignOut
import qualified EdgeNode.Controller.Auth.RefreshAccessToken as Auth.RefreshAccessToken
import qualified EdgeNode.Controller.Provider.Publish as Provider.Publish
import qualified EdgeNode.Controller.Provider.QualificationBuilder.GetAvailableBranches as QualificationBuilder.GetAvailableBranches
import qualified EdgeNode.Controller.Provider.QualificationBuilder.Create as QualificationBuilder.Create
import qualified EdgeNode.Controller.Provider.QualificationBuilder.GetAreaToCountries as QualificationBuilder.GetAreaToCountries
import qualified EdgeNode.Controller.Provider.QualificationBuilder.GetCountryToTypes as QualificationBuilder.GetCountryToTypes
import qualified EdgeNode.Controller.Provider.QualificationBuilder.GetTypeToQualifications as QualificationBuilder.GetTypeToQualifications
import qualified EdgeNode.Controller.Provider.GetQualifications as Provider.GetQualifications
import qualified EdgeNode.Controller.Provider.GetQualification as Provider.GetQualification
import qualified EdgeNode.Controller.Provider.PatchQualification as Provider.PatchQualification
import qualified EdgeNode.Controller.User.GetProfile as User.GetProfile
import qualified EdgeNode.Controller.User.PatchProfile as User.PatchProfile
import qualified EdgeNode.Controller.User.Trajectory.Add as User.AddTrajectory
import qualified EdgeNode.Controller.User.Qualification.AddQualifications as User.AddQualifications
import qualified EdgeNode.Controller.User.Qualification.GetDegreeTypesByCategory as User.GetDegreeTypesByCategory
import qualified EdgeNode.Controller.User.Qualification.GetCountriesByDegreeType as User.GetCountriesByDegreeType
import qualified EdgeNode.Controller.User.Qualification.GetBranchesByCountry as User.GetBranchesByCountry
import qualified EdgeNode.Controller.User.Qualification.GetQualificationsByBranch as User.GetQualificationsByBranch
import qualified EdgeNode.Controller.User.Qualification.GetQualificationList as User.GetQualificationList
import qualified EdgeNode.Controller.User.Qualification.PurgeQualifications as User.PurgeQualifications
import qualified EdgeNode.Controller.User.Trajectory.GetList as User.GetTrajectories
import qualified EdgeNode.Controller.User.Trajectory.Remove as User.Trajectory.Remove
import qualified EdgeNode.Controller.Feedback.Put as Feedback.Put
import qualified EdgeNode.Controller.Service.Enum.GetCountries as Enum.GetCountries
import qualified EdgeNode.Controller.Service.Enum.GetQualification as Enum.GetQualification
import qualified EdgeNode.Controller.Statistics.GetActiveUsers as Statistics.GetActiveUsers
import qualified EdgeNode.Controller.Statistics.GetRegistrations as Statistics.GetRegistrations
import qualified EdgeNode.Controller.Statistics.GetApiCounter as Statistics.GetApiCounter

import Auth
import Katip
import KatipController
import Servant.Server.Generic
import Servant.API.Generic
import Servant.Auth.Server
import Control.Lens
import Database.Transaction
import Data.Bool
import qualified Data.Text as T
import BuildInfo

controller :: ApplicationApi (AsServerT KatipController)
controller = ApplicationApi { _applicationApiHttp = toServant httpApi }

verifyAuthorization :: UserId -> Rbac.Permission -> (UserId -> KatipController (Response a)) -> KatipController (Response a)
verifyAuthorization user_id perm controller = do
  runTelegram $location $
    "authorization check: user {" <>
    show user_id <>
    "}, permission {" <>
    show perm <> "}"
  hasql <- (^.katipEnv.hasqlDbPool) `fmap` ask
  isOk <- katipTransaction hasql $ do
    xs <- statement Rbac.getTopLevelRoles (user_id, perm)
    statement Rbac.isPermissionBelongToRole (xs, perm)
  let error = "you have no permissions to fulfill this operation"
  bool (return (Error (asError @T.Text error))) (controller user_id) isOk

httpApi :: HttpApi (AsServerT KatipController)
httpApi =
  HttpApi
  { _httpApiAuth       = toServant auth
  , _httpApiUser       = (`withAuthResult` (toServant . user))
  , _httpApiSearch     = toServant search
  , _httpApiFile       = toServant file
  , _httpApiAdmin      = (`withAuthResult` (toServant . admin))
  , _httpApiProvider   = (`withAuthResult` (toServant . provider))
  , _httpApiFeedback   = toServant feedback
  , _httpApiService    = toServant service
  , _httpApiSite       = toServant site
  , _httpApiStatistics = (`withAuthResult` (toServant . statistics))
  }

site :: SiteApi (AsServerT KatipController)
site= SiteApi { _siteApiNews = toServant news }

news :: NewsApi (AsServerT KatipController)
news =
  NewsApi
  { _newsApiGetNewsFeed = undefined
  , _newsApiGetNewsItem = undefined
  }

service :: ServiceApi (AsServerT KatipController)
service = ServiceApi { _serviceApiEnum = toServant EdgeNode.Controller.Controller.enum }

enum :: EnumApi (AsServerT KatipController)
enum =
  EnumApi
  { _enumApiGetCountries = \lang ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["service", "enum", "countries"])
    (Enum.GetCountries.controller lang)
  , _enumApiGetQualificationEnum = \lang ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["service", "enum", "qualification"])
    (Enum.GetQualification.controller lang)
  }

feedback :: FeedbackApi (AsServerT KatipController)
feedback =
  FeedbackApi
  { _feedbackApiPutFeedback = \feedback ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["feedback"])
    (Feedback.Put.controller feedback)
  }

auth :: AuthApi (AsServerT KatipController)
auth =
  AuthApi
  { _authApiAuthentication = \credential ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["auth", "authentication"])
    (Auth.SignIn.controller credential)
  , _authApiRefreshAccessToken = \uid token ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["auth", "refreshAccessToken"])
    (Auth.RefreshAccessToken.controller token uid)
  , _authApiLogout = \user_id refresh_token_hash ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["auth", "logout"])
    (Auth.SignOut.controller user_id refresh_token_hash)
  , _authApiRegistration = \registration ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["auth", "registration"])
    (Auth.Registration.controller registration)
  }

user :: AuthResult JWTUser -> UserApi (AsServerT KatipController)
user user =
  UserApi
  { _userApiGetProfile =
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["user", "profile"])
    (applyController Nothing user $ \x ->
     verifyAuthorization
     (jWTUserUserId x)
     Rbac.PermissionUser
     (User.GetProfile.controller))
  , _userApiPatchProfile = \profile ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["user", "profile"])
    (applyController Nothing user $ \x ->
     verifyAuthorization
     (jWTUserUserId x)
     Rbac.PermissionUser
     (User.PatchProfile.controller profile))
  , _userApiTrajectory = toServant (trajectory user)
  , _userApiQualification = toServant (userQualification user)
  }

trajectory :: AuthResult JWTUser -> TrajectoryApi (AsServerT KatipController)
trajectory user =
  TrajectoryApi
  { _trajectoryApiAddTrajaectory = \trajectory ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["user", "trajectory", "add"])
    (applyController Nothing user $ \x ->
     verifyAuthorization
     (jWTUserUserId x)
     Rbac.PermissionUser
     (User.AddTrajectory.controller trajectory))
  , _trajectoryApiGetTrajaectories =
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["user", "trajectory", "list"])
    (applyController Nothing user $ \x ->
     verifyAuthorization
     (jWTUserUserId x)
     Rbac.PermissionUser
     (User.GetTrajectories.controller))
  , _trajectoryApiDeleteTrajaectory = \trajectory_id ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["user", "trajectory", "remove"])
    (applyController Nothing user $ \x ->
     verifyAuthorization
     (jWTUserUserId x)
     Rbac.PermissionUser
     (User.Trajectory.Remove.controller trajectory_id))
  }

userQualification :: AuthResult JWTUser -> UserQualificationApi (AsServerT KatipController)
userQualification user =
  UserQualificationApi
  { _userQualificationApiGetDegreeTypesByCategory = \category ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["user", "qualification", "category"])
    (applyController Nothing user $ \x ->
     verifyAuthorization
     (jWTUserUserId x)
     Rbac.PermissionUser
     (const (User.GetDegreeTypesByCategory.controller category)))
  , _userQualificationApiGetCountriesByDegreeType =
    \category degree_type ->
      flip logExceptionM ErrorS $
      katipAddNamespace
      (Namespace ["user", "qualification", "category, degree_type"])
      (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionUser
       (const (User.GetCountriesByDegreeType.controller category degree_type)))
  , _userQualificationApiGetBranchesByCountry =
    \category degree_type country ->
      flip logExceptionM ErrorS $
      katipAddNamespace
      (Namespace ["user", "qualification", "category degree_type, country"])
      (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionUser
       (const (User.GetBranchesByCountry.controller category degree_type country)))
  , _userQualificationApiGetQualificationsByBranch = \branch_id degree_type ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["user", "qualification", "list"])
     (applyController Nothing user $ \x ->
      verifyAuthorization
      (jWTUserUserId x)
      Rbac.PermissionUser
      (User.GetQualificationsByBranch.controller branch_id degree_type))
  , _userQualificationApiAddQualificationToTrajectory =
    \qualifications ->
      flip logExceptionM ErrorS $
      katipAddNamespace
      (Namespace ["user", "qualificaton", "add"])
      (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionUser
       (User.AddQualifications.controller qualifications))
  , _userQualificationApiGetQualificationList =
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["user", "qualificaton", "list"])
    (applyController Nothing user $ \x ->
     verifyAuthorization
     (jWTUserUserId x)
     Rbac.PermissionUser
     (User.GetQualificationList.controller))
  , _userQualificationApiPurgeQualifications =
    \provider_id qualification_list ->
      flip logExceptionM ErrorS $
      katipAddNamespace
      (Namespace ["user", "qualificaton", "purge"])
      (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionUser
       (User.PurgeQualifications.controller provider_id qualification_list))
  }

search :: SearchApi (AsServerT KatipController)
search =
  SearchApi
  { _searchApiGetSearchBar = \query ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["search", "bar"])
     (Search.GetBarItems.controller query)
  , _searchApiGetQualificationList = \user query -> do
    let unauthorized = Search.GetQualificationList.unauthorizedController
    let authorized = Search.GetQualificationList.authorizedController
    flip logExceptionM ErrorS $
      katipAddNamespace
      (Namespace ["search", "qualification", "list"])
      (applyController (Just (unauthorized query))
        user $ \x -> authorized query (jWTUserUserId x))
  , _searchApiGetQualificationModal = \qualification_id ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["search", "qualification", "modal"])
    (Search.GetQualificationModal.controller qualification_id)
  }

file :: FileApi (AsServerT KatipController)
file =
  FileApi
  { _fileApiUpload = \bucket files ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["file", "upload"])
    (File.Upload.controller bucket files)
  , _fileApiPatch = \fid file ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["file", "patch"])
    (File.Patch.controller fid file)
  , _fileApiDelete = \fid ->
     flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["file", "delete"])
     (File.Delete.controller fid)
  , _fileApiDownload = \option fid w h ->
     flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["file", "download"])
     (File.Download.controller option fid w h)
  }

admin :: AuthResult BasicUser -> AdminApi (AsServerT KatipController)
admin user =
  AdminApi
  { _adminApiProviderRegister = \provider ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["admin", "provider", "register"])
     (withUser user (Admin.ProviderRegister.controller provider))
  , _adminApiResetPassword = \provider_id email ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["admin", "provider", "password", "reset"])
     (withUser user (const (Admin.ResetPassword.controller provider_id email)))
  }

provider :: AuthResult JWTUser -> ProviderApi (AsServerT KatipController)
provider user =
  ProviderApi
  { _providerApiGetBranches =
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["provider", "branch", "list"])
     (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionProviderGuest
       Provider.GetBranches.controller)
  , _providerApiCreateBranches = \branch ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["provider", "branch", "create"])
     (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionProviderAdmin
       (Provider.CreateBranches.controller branch))
  , _providerApiPatchBranches = \branches ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["provider", "branch", "patch"])
     (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionProviderAdmin
       (Provider.PatchBranch.controller branches))
  , _providerApiDeleteBranch = \ident ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["provider", "branch", "delete"])
     (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionProviderAdmin
       (Provider.DeleteBranch.controller ident))
  , _providerApiSetHQ = \ident ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["provider", "branch", "hq"])
     (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionProviderAdmin
       (Provider.SetHQ.controller ident))
  , _providerApiBuilderCreateQualification = \builder ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["provider", "qualification", "builder", "create"])
     (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionProviderAdmin
       (QualificationBuilder.Create.controller builder))
  , _providerApiBuilderGetAvailableBranches =
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["provider", "qualification", "builder", "getBranches"])
     (applyController Nothing user $ \x ->
       verifyAuthorization
       (jWTUserUserId x)
       Rbac.PermissionProviderAdmin
       QualificationBuilder.GetAvailableBranches.controller)
  , _providerApiPublish =
    flip logExceptionM ErrorS $
    katipAddNamespace
     (Namespace ["provider", "branch", "publish"])
     (applyController Nothing user $ \x ->
      verifyAuthorization
      (jWTUserUserId x)
      Rbac.PermissionProviderAdmin
      Provider.Publish.controller)
  , _providerApiBuilderGetDependencyAreaToCountries = \area ->
    flip logExceptionM ErrorS $
    katipAddNamespace
     (Namespace ["provider", "qualification", "builder", "areaToCountries"])
     (applyController Nothing user $ \x ->
      verifyAuthorization
      (jWTUserUserId x)
      Rbac.PermissionProviderAdmin
      (const (QualificationBuilder.GetAreaToCountries.controller area)))
  , _providerApiBuilderGetDependencyCountryToTypes = \area cntry ->
    flip logExceptionM ErrorS $
    katipAddNamespace
     (Namespace ["provider", "qualification", "builder", "CountryToTypes"])
     (applyController Nothing user $ \x ->
      verifyAuthorization
      (jWTUserUserId x)
      Rbac.PermissionProviderAdmin
      (const (QualificationBuilder.GetCountryToTypes.controller area cntry)))
  , _providerApiBuilderGetDependencTypeToQualifications = \area cntry degree ->
    flip logExceptionM ErrorS $
    katipAddNamespace
     (Namespace ["provider", "qualification", "builder", "TypeToQualifications"])
     (applyController Nothing user $ \x ->
      verifyAuthorization
      (jWTUserUserId x)
      Rbac.PermissionProviderAdmin
      (const (QualificationBuilder.GetTypeToQualifications.controller area cntry degree)))
  , _providerApiGetQualifications =
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["provider", "qualification", "list"])
     (applyController Nothing user $ \x ->
      verifyAuthorization
      (jWTUserUserId x)
      Rbac.PermissionProviderGuest
      Provider.GetQualifications.controller)
  , _providerApiGetQualification = \ident ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["provider", "qualification", "get"])
     (applyController Nothing user $ \x ->
      verifyAuthorization
      (jWTUserUserId x)
      Rbac.PermissionProviderAdmin
      (Provider.GetQualification.controller ident))
  , _providerApiPatchQualification = \ident patch ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["provider", "qualification", "patch"])
     (applyController Nothing user $ \x ->
      verifyAuthorization
      (jWTUserUserId x)
      Rbac.PermissionProviderAdmin
      (Provider.PatchQualification.controller ident patch))
  , _providerApiPool = toServant (pool user)
  }

pool :: AuthResult JWTUser -> PoolApi (AsServerT KatipController)
pool user = PoolApi { _poolApiTags = toServant (tags user) }

tags :: AuthResult JWTUser -> TagsApi (AsServerT KatipController)
tags _ = TagsApi { _tagsApiCreate = undefined }

statistics :: AuthResult BasicUser -> StatisticsApi (AsServerT KatipController)
statistics user =
  StatisticsApi
  { _statisticsApiGetRegistrations = \from ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["statistics", "registrations"])
     (withUser user (const (Statistics.GetRegistrations.controller from)))
  , _statisticsApiGetActiveUsers = \from ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["statistics", "users"])
     (withUser user (const (Statistics.GetActiveUsers.controller from)))
  , _statisticsApiGetApiCounter = \from ->
    flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["statistics", "api"])
     (withUser user (const (Statistics.GetApiCounter.controller from)))
  }