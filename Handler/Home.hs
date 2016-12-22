module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import DbUtils

-- Обработчик GET запроса
getHomeR :: Handler Html
getHomeR = do 
        sponsors <- (runDB $ getSponsors)

        defaultLayout [whamlet|
        <div .masthead>
            <div .container>
                 <h1>
                    Добро пожаловать на конференцию 2 0 1 6
                 <a href="http://localhost:3000/aboutconf/" .btn.btn-info.btn-lg>
                        Условие конференции

       <div .container>
        <h1> Спонсоры мероприятия </h1>
            $forall (Sponsor n (Just r)) <- dropEntityList sponsors   
                <div class="thumbs">
                    <td> <a href= "#{r}" target="_blank">
                        <img src="static/sponsor/#{n}.jpg"/></a>  </td>
                        <div class="caption">
                            <span class="title">#{n}</span>   
                            <p>
                            <p> 

         <a class="boxed" href="mailto:email@whatever.com?subject=Partnership">Стать партнером</a>

         <h1> Участники мероприятия</h1>

        

        <script src="//yastatic.net/es5-shims/0.0.2/es5-shims.min.js"></script>
        <script src="//yastatic.net/share2/share.js"></script>
        <div class="ya-share2" data-services="collections,vkontakte,facebook,odnoklassniki,moimir" data-counter=""></div>
    |]
