module Handler.Program where

import Import
import DbUtils
import qualified System.IO as SIO
import Text.Read
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Control.Monad
import Database.Persist.Sql

getProgramR :: Handler Html
getProgramR = do
     rep <- runDB $ getReports
     let rs =  fmap (\(Entity _ x) -> (reportTitle x, reportReporter x, reportTime x, reportDay x, reportRoom x, reportSeats x)) rep
     defaultLayout [whamlet|
     <center>
            <table border="2" bordercolor="black" width="80%" cellpadding="10" cellspacing="40" bgcolor="#0000FF">
                    <thead>
                        <tr>
                        <th bgcolor="#FFFF00">Название  </th>
                        <th bgcolor="#FFFF00">Докладчик </th>
                        <th bgcolor="#FFFF00">Время мероприятия </th>
                        <th bgcolor="#FFFF00" >Дата мероприятия </th>
                        <th bgcolor="#FFFF00">Аудитория </th>
                        <th bgcolor="#FFFF00">Место </th>
                    <tbody>
                        $forall (name, repr, tm, dy, roo, sts) <- rs
                            <tr>
                                <td>#{name}</td>
                                <td>#{repr}</td>
                                <td>#{tm}</td>
                                <td>#{dy}</td>
                                <td>#{show $ fromSqlKey  roo}</td>
                                <td>#{show sts}</td>
                               
                
    |]


postProgramR :: Handler Html
postProgramR = do 
     defaultLayout [whamlet|
            <h1>Hello World!
            <table>
                <tr>
                    <td> Text
                    <td> Blah
                <tr> 
                    <td> 111
    |]

