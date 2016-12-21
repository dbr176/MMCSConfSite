module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import DbUtils

-- Обработчик GET запроса
getHomeR :: Handler Html
getHomeR = do 
        sponsors <- (runDB $ getSponsors)

        defaultLayout [whamlet|<div .container>


        <tbody>
        $forall (Sponsor n (Just r)) <- dropEntityList sponsors   
            <td> <a href= "#{r}" target="_blank">
                <img src="static/sponsor/#{n}.jpg" width="250" height="250"> </td></a>  
            <td>#{n}</td>  
            <p>
        
                   

        <a class="boxed" href="mailto:email@whatever.com?subject=Partnership">Стать партнером</a>

        <script src="//yastatic.net/es5-shims/0.0.2/es5-shims.min.js"></script>
        <script src="//yastatic.net/share2/share.js"></script>
        <div class="ya-share2" data-services="collections,vkontakte,facebook,odnoklassniki,moimir" data-counter=""></div>
    |]
