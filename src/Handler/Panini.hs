{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, FlexibleContexts, GADTs  #-}
module Handler.Panini where

import Import
import Database.Persist.Sql
import Yesod.Form.Bootstrap3 (BootstrapFormLayout(..),renderBootstrap3, bfs)
import Yesod.Form.Nic (nicHtmlField)
import Yesod

signUpForm :: Form User
signUpForm = renderBootstrap3 BootstrapBasicForm $ User
          <$> areq textField "Username: " Nothing
          <*> aopt passwordField "Password: " Nothing
          <*> areq doubleField "Longitude: " Nothing
          <*> areq doubleField "Latitude: " Nothing



getUserLaminaR :: Handler Html
getUserLaminaR = do
    allComments <- runDB $ rawSql s []
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "paninipage")
            where s = "SELECT ?? FROM (SELECT lamina as id,lamina as lamina, CAST(SUM(cantidad) as int8) as cantidad FROM user_lamina GROUP BY lamina) table_lamina ORDER BY lamina"

getSignUpR :: Handler Html
getSignUpR = do
    (userWidget, enctype) <- generateFormPost signUpForm
    defaultLayout $ do
        setTitle "Sign up"
        $(widgetFile "signuppage")
        
postSignUpR :: Handler Html
postSignUpR = do
    ((res,userWidget), enctype) <- runFormPost signUpForm
    case res of
        FormSuccess user -> do
            entryId <- runDB $ insert user
            redirect $ UserLaminaR
        _ -> defaultLayout $ do
            setMessage $ "Please, correct de form"
            $(widgetFile "signuppage")


