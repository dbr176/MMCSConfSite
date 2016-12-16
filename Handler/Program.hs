module Handler.Program where

import Import
import DbUtils
import qualified System.IO as SIO
import Text.Read
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

getProgramR :: Handler Html
getProgramR = do
     defaultLayout [whamlet|
            <table>
                <thead>
                    <tr>
                    <th>First Name</th>
                    <th>Last Name</th>
                    <th>Age</th>
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

