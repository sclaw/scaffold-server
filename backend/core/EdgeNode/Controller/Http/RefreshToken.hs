module EdgeNode.Controller.Http.RefreshToken (controller) where

import qualified EdgeNode.Api.Http.Auth.RefreshToken as RefreshToken ()

import RetrofitProto
import ReliefJsonData
import KatipController

controller :: RefreshTokenRequest -> KatipController (Alternative ErrorRefreshToken RefreshTokenResponse)
controller = undefined
