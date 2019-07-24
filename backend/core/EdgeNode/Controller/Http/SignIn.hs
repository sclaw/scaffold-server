module EdgeNode.Controller.Http.SignIn (controller) where

import qualified EdgeNode.Api.Http.Auth.SignIn as SignIn

import RetrofitProto
import ReliefJsonData
import KatipController

controller :: SignIn.Request -> KatipController(Alternative ErrorSignIn SignIn.Response)
controller = undefined