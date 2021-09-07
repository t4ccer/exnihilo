module Exnihilo.Parser where

-- parseInput :: Text -> Input
-- parseInput = Input . map mconcat . groupBy isText . fromJust . parseMaybe inpP

-- inpP :: Parser [InputPart]
-- inpP = many $ choice [try varP, try textP]

-- varP :: Parser InputPart
-- varP = string "{{" *> (InputVar . T.pack <$> many (anySingleBut '}')) <* string "}}"

-- textP :: Parser InputPart
-- textP = InputText . T.singleton <$> anySingle
