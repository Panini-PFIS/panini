{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    user <- maybeAuth 
    allComments <- runDB $ getLaminasQuery
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "paninipage")
--            where s = "SELECT ?? FROM (SELECT lamina as id,lamina as lamina, CAST(SUM(cantidad) as int8) as cantidad FROM user_lamina GROUP BY lamina) table_lamina ORDER BY lamina"

getLaminasQuery :: MonadIO m => ReaderT SqlBackend m [(Single Int, Single Int)]
getLaminasQuery = rawSql s []
                    where s = "select lamina, cast(sum(cantidad) as int8) as cantidad \
                              \from user_lamina group by lamina order by lamina"

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
            setMessage $ "Please, correct the form"
            $(widgetFile "signuppage")

exchangeForm :: UserId -> UserId -> Form Intercambio
exchangeForm user1 user2 = renderBootstrap3 BootstrapBasicForm $ Intercambio
        <$> pure user1
        <*> pure user2
        <*> areq intField (bfs MsgLamina1) Nothing
        <*> areq intField (bfs MsgLamina2) Nothing
        <*> lift (liftIO getCurrentTime)

getExchangeR :: UserId -> UserId -> Handler Html
getExchangeR _ _ = do
    exchanges <- runDB $ getExchangeQuery
    defaultLayout $ do
        setTitleI $ MsgExchangeTitle
        $(widgetFile "exchangepage")

getExchangeQuery :: MonadIO m => ReaderT SqlBackend m [(Entity Intercambio, Single Text, Single Text)]
getExchangeQuery = rawSql s []
                        where s = "select ??,\"user\".ident,us.ident \
                                  \from intercambio left join \"user\" on intercambio.user1 = \"user\".id \
                                  \left join \"user\" as us on intercambio.user2 = us.id"

postExchangeR :: UserId -> UserId -> Handler Html
postExchangeR user1 user2 = do
    ((res,exchangeWidget), enctype) <- runFormPost (exchangeForm user1 user2)
    case res of
        FormSuccess exchange -> do
            _ <- runDB $ insert exchange
            redirect UserLaminaR
        _ -> defaultLayout $ do
            setMessageI $ MsgUnsuccessfulExchange
            $(widgetFile "exchangeform")
