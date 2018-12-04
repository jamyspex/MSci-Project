module CmdCFlagsParser where

import           Data.Attoparsec.Text

data CPPDefine =
    CPPDefine {
        name  :: String,
        value :: Maybe String
    }

-- parseCPPDefines :: Parser [CPPDefine]

parseCPPDefine :: Parser CPPDefine
parseCPPDefine = do
    name <- takeTill (\c -> c == '=' || c == ',')
    value <- Nothing
    return $ CPPDefine name value

-- parseString :: Parser Text
-- parseCPPValue :: Parser (Maybe String)
-- parseCPPValue = do
--     char '='
--     value <- takeTill (==',')
--     return value
