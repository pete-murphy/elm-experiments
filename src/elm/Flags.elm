module Flags exposing (..)

import DeviceId exposing (DeviceId)
import SessionId exposing (SessionId)
import Time
import UserId exposing (UserId)


type alias PartialFlags =
    { deviceId : Maybe DeviceId
    , sessionId : Maybe SessionId
    , userId : Maybe UserId
    , now : Maybe Time.Posix
    , timeZone : Maybe Time.Zone
    }


type alias Flags =
    { deviceId : DeviceId
    , sessionId : SessionId
    , userId : UserId
    , now : Time.Posix
    , timeZone : Time.Zone
    }
