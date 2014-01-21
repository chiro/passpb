{-# LANGUAGE OverloadedStrings #-}
module Web.Passpb.Route where

import Data.Text (Text)

data MyRoute =
  Home
  | Login
  | Logout
  | Register
  | List

render :: MyRoute -> [(Text, Text)] -> Text
render route _ = getPath route

getPath :: MyRoute -> Text
getPath Home = "/"
getPath Login = "/login"
getPath Logout = "/logout"
getPath Register = "/register"
getPath List = "/list"
