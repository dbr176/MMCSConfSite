module Handler.Program where

import Import
import DbUtils
import qualified System.IO as SIO
import Text.Read
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Control.Monad

getProgramR :: Handler Html
getProgramR = do
     rep <- runDB $ getReports
     let rs =  fmap (\(Entity _ x) -> (reportTitle x, reportReporter x)) rep
     defaultLayout [whamlet|
            <table>
                    <thead>
                        <tr>
                        <th>Название  </th>
                        <th>Докладчик </th>
                    <tbody>
                        $forall (idn, sts) <- rs
                            <tr>
                                <td>#{idn}</td>
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

