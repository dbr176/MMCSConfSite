module Handler.Admin where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

data LoadRoomsForm = LoadRoomsForm {
    fileInfo :: FileInfo
}

getAdminR :: Handler Html
getAdminR = do 
    (widget, enctype) <- generateFormPost roomsForm
    defaultLayout
        [whamlet|
            <p>
                The widget generated contains only the contents
                of the form, not the form tag itself. So...
            <form method=post action=@{AdminR} enctype=#{enctype}>
                ^{widget}
                <p>It also doesn't include the submit button.
                <button>Submit
        |]

postAdminR :: Handler Html
postAdminR = do 
    ((result, widget), enctype) <- runFormPost roomsForm
    case result of
        FormSuccess file -> 
            -- Нужно делать что-то с file
            defaultLayout [whamlet|<p>Загружено|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{AdminR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

roomsForm :: Form LoadRoomsForm
roomsForm = do
    renderBootstrap3 BootstrapBasicForm $ LoadRoomsForm 
                                       <$> fileAFormReq "Choose a file"
