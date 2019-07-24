module EdgeNode.Controller.Http.SignIn (controller) where

import qualified EdgeNode.Api.Http.Auth.SignIn as SignIn ()

import RetrofitProto
import ReliefJsonData
import KatipController

controller :: SignInRequest -> KatipController (Alternative ErrorSignIn SignInResponse)
controller = undefined