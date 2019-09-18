-- incomplete implementation. making a commit initially
-- to snapshot whatever is working. Fix this module!

module AhoCorasick
    ( buildMachine
    , findAll
    )
where

import qualified Data.Text                     as Text
import qualified Data.HashMap.Strict           as Map

type Stage = Int

-- incorrect.
newtype Machine = Machine (Map.HashMap Int Machine)

buildMachine :: [Text.Text] -> Machine
buildMachine = undefined

findAll :: Machine -> Text.Text -> [Position]
findAll = undefined

data Position = Position
    {
        start :: Int
    ,   end :: Int
    ,   val :: Text.Text
    } deriving(Show)
