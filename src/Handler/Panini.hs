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

exchangeForm :: UserId -> UserId -> Int -> Form Intercambio
exchangeForm user1 user2 lamina2 = renderBootstrap3 BootstrapBasicForm $ Intercambio
        <$> pure user1
        <*> pure user2
        <*> areq (radioField laminasopt) "Lamina" Nothing
        <*> pure lamina2
        <*> areq intField "Cantidad: " Nothing
        <*> lift (liftIO getCurrentTime)
        where   laminasopt = do
                    entities <- runDB $ selectList [UserLaminaUser ==. user1] [Asc UserLaminaLamina]
                    optionsPairs $ map (\lamina -> (MsgLaminaOption $ userLaminaLamina $ entityVal $ lamina, userLaminaLamina $ entityVal lamina)) entities

getExchangeR :: UserId -> UserId -> Int -> Handler Html
getExchangeR user1 user2 lamina = do
    (exchangeWidget, enctype) <- generateFormPost (exchangeForm user1 user2 lamina)
    exchanges <- runDB $ getExchangeQuery
    defaultLayout $ do
        setTitleI $ MsgExchangeTitle
        $(widgetFile "exchangeform")

getExchangeQuery :: MonadIO m => ReaderT SqlBackend m [(Entity Intercambio, Single Text, Single Text)]
getExchangeQuery = rawSql s []
                        where s = "select ??,\"user\".ident,us.ident \
                                  \from intercambio left join \"user\" on intercambio.user1 = \"user\".id \
                                  \left join \"user\" as us on intercambio.user2 = us.id"

postExchangeR :: UserId -> UserId -> Int -> Handler Html
postExchangeR user1 user2 lamina = do
    ((res,exchangeWidget), enctype) <- runFormPost (exchangeForm user1 user2 lamina)
    case res of
        FormSuccess exchange -> do
            runDB $ do
                let l2 = intercambioLamina2 exchange
                let c = intercambioCantidad exchange
                updateWhere [UserLaminaUser ==. user1, UserLaminaLamina ==. lamina] [UserLaminaCantidad -=. c]
                updateWhere [UserLaminaUser ==. user2, UserLaminaLamina ==. l2] [UserLaminaCantidad -=. c]
                ans <- getBy $ UniqueUserLamina user1 l2
                _ <- case ans of
                    Nothing -> insert $ UserLamina user1 l2 c
                    _ -> do
                            updateWhere [UserLaminaUser ==. user1, UserLaminaLamina ==. l2] [UserLaminaCantidad +=. c]
                            return $ toSqlKey 0
                ans2 <- getBy $ UniqueUserLamina user1 l2
                _ <- case ans2 of
                    Nothing -> insert $ UserLamina user2 lamina c
                    _ -> do
                            updateWhere [UserLaminaUser ==. user2, UserLaminaLamina ==. lamina] [UserLaminaCantidad +=. c]
                            return $ toSqlKey 0
                deleteWhere [UserLaminaCantidad <=. 0]
            redirect UserLaminaR
        _ -> defaultLayout $ do
            setMessageI $ MsgUnsuccessfulExchange
            $(widgetFile "exchangeform")

getLaminaR :: Int -> Handler Html
getLaminaR lamina = do
    useractual <- maybeAuth 
    laminas <- runDB $ getLaminaUsuariosQuery $ lamina
    defaultLayout $ do
        setTitleI $ MsgExchangeTitle
        $(widgetFile "laminaseleccionadausuarios")

getLaminaUsuariosQuery :: MonadIO m => Int -> (ReaderT SqlBackend m [(Entity User, Single Int)])
getLaminaUsuariosQuery lamina = rawSql s []
                        where s = pack $ "select ??,cantidad from user_lamina left join \"user\" on user_lamina.user = \"user\".id where lamina = " ++ (show lamina)
