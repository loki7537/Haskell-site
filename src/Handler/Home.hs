{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

-- Определяем наши данные, которые будут использоваться для создания формы.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- Это функция-обработчик для метода запроса GET на HomeR.
-- шаблон ресурса. Все ваши шаблоны ресурсов определены в
-- config/routes.yesodroutes
--
-- Большая часть кода, который вы будете писать в Yesod, находится в этих обработчиках.
-- функции. Вы можете распределить их по нескольким файлам, если хотите, или создать один монолитный файл.
getHomeR :: Handler Html
getHomeR = do -- точка входа:
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    allComments <- runDB $ getAllComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Дендрологи" --надпись вкладки браузера
        $(widgetFile "homepage") -- все файлы Шекспира с этим именем 

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    allComments <- runDB $ getAllComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Дендрологи"
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Выберете файл"
    <*> areq textField textSettings Nothing
    --Добавьте атрибуты, такие как наполнение и классы CSS..
    where textSettings = FieldSettings
            { fsLabel = "Что в файле?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "Описание файла")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]
