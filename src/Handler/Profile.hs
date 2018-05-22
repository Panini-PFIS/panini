{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import Database.Persist.Sql
import Yesod.Form.Bootstrap3 (BootstrapFormLayout(..),renderBootstrap3, bfs)
import Yesod.Form.Nic (nicHtmlField)
import Yesod


formNewLamina :: UserId -> Form UserLamina
formNewLamina userId = renderBootstrap3 BootstrapBasicForm $ UserLamina
          <$> pure  userId
          <*> areq intField "Lamina: " Nothing
          <*> areq intField "Cantidad: " Nothing


getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    idUserArray <- runDB $ (getUserId (userIdent user))
    idUser <- getHeadArray idUserArray
    userLaminas <- runDB $ (getUserLaminasQuery (userIdent user))
    (newLaminaWidget, enctype) <- generateFormPost (formNewLamina idUser)
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = do
    (_, user) <- requireAuthPair
    idUserArray <- runDB $ (getUserId (userIdent user))
    idUser <- getHeadArray idUserArray
    userLaminas <- runDB $ (getUserLaminasQuery (userIdent user))
    ((res,newLaminaWidget), enctype) <- runFormPost (formNewLamina idUser)
    case res of
        FormSuccess userLamina  -> do
            runDB $ insert userLamina
            redirect $ ProfileR
        _ -> defaultLayout $ do
            setMessage $ "Please, correct the form"
            $(widgetFile "profile")

getHeadArray :: [UserId] -> HandlerFor App UserId
getHeadArray (x:_)= return x 

getUserId :: MonadIO m => Text ->  ReaderT SqlBackend m [UserId]
getUserId user = rawSql s []
    where s = "select id from public.user where ident = '"++ user ++"'"
getUserLaminasQuery :: MonadIO m => Text ->  ReaderT SqlBackend m [(Single Int, Single Int)]
getUserLaminasQuery user = rawSql s []
    where s = "select lamina,cantidad \
            \from user_lamina where user_lamina.user = (select id from public.user where ident = '"++ user ++"') "

getCantidadR :: Int -> Int -> Handler Html
getCantidadR lamina cantidad = do
    defaultLayout $ do
        setTitle  $ "Change lamina"
        $(widgetFile "change")

getCantidadCambiadaR :: Int -> Int -> Handler Html
getCantidadCambiadaR lamina cantidad= do
    (_, user) <- requireAuthPair
    idUserArray <- runDB $ (getUserId (userIdent user))
    idUser <- getHeadArray idUserArray
    userLaminas <- runDB $ (getUserLaminasQuery (userIdent user))
    defaultLayout $ do
        setTitle  $ "Change lamina total"
        $(widgetFile "changeTotal")
    --runDB $ update (UniqueUserLamina idUser lamina) [cantidad =. userLamina $ cantidad]
    --redirect $ ProfileR    
    
