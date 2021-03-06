module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import DbUtils
import Database.Persist.Sql as SQL
import Data.Maybe

-- Обработчик GET запроса
getHomeR :: Handler Html
getHomeR = do 
        sponsors <- runDB $ getSponsors
        members <- runDB $ getUsersInfo
        let ms =  dropEntityList members
        let replaceO = map (\c -> if c=='_' then ' ' else c)
        rms <- sequence $ map (\(UserInfo r _) ->  runDB $ SQL.get r) ms
        let msr = zipWith (,) ms rms

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
                        <img src="static/sponsor/#{n}.jpg"/></a></td>
                        <div class="caption">
                            <span class="title">#{n}</span>   
                            <p>
                            <p> 

         <a class="boxed" href="mailto:email@whatever.com?subject=Partnership">Стать партнером</a>

         <h1> Участники мероприятия</h1>
            $forall ((UserInfo _ _) , Just (User idn about)) <- msr  
                <div class="thumbs">
                    <td><img src="static/members/#{idn}.jpg"/></td>
                        <div class="caption">
                            <span class="title">#{replaceO $ (unpack idn) ++ (unpack $ "\n" ++ (fromJust $ about))}</span>   
                            <p>
                            <p> 
        

        <script src="//yastatic.net/es5-shims/0.0.2/es5-shims.min.js"></script>
        <script src="//yastatic.net/share2/share.js"></script>
        <div class="ya-share2" data-services="collections,vkontakte,facebook,odnoklassniki,moimir" data-counter=""></div>
    |]
