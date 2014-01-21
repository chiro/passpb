{-# LANGUAGE OverloadedStrings #-}
module Route where

import Data.Text (Text)

data MyRoute =
  Home
  | Login
  | Logout
  | Register

render :: MyRoute -> [(Text, Text)] -> Text
render Home _ = "/"
render Login _ = "/login"
render Logout _ = "/logout"
render Register _ = "/register"
