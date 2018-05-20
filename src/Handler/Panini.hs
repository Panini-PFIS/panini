{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, FlexibleContexts, GADTs  #-}
module Handler.Panini where

import Import
import Database.Persist.Sql

getUserLaminaR :: Handler Html
getUserLaminaR = do

    allComments <- runDB $ rawSql s []
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "paninipage")
            where s = "SELECT ?? FROM (SELECT lamina as id,lamina as lamina, CAST(SUM(cantidad) as int8) as cantidad FROM user_lamina GROUP BY lamina) table_lamina ORDER BY lamina"

{-
getUserLaminaR :: Handler Html
getUserLaminaR = do
    allComments <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "paninipage")



<table>
        <thead>
            <tr>
                <th>Lamina</th> 
                <th>Cantidad</th>
        <tbody>
            $forall Entity tableLaminaid tableLamina <- allComments
                <tr>
                    <td>#{tableLaminaLamina tableLamina}
                    <td>#{tableLaminaCantidad tableLamina}
-}
