module Parser.ParseDateTime ( parseDateTime ) where

import Data.Time


parseDateTime :: String -> ZonedTime
parseDateTime str 
  | length str == 25 && str !! 10 == 'T' = 
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" str of
      Just zonedTime -> zonedTime
      Nothing -> error ("Parsing for the 1st case of time formats has failed: " ++ str)
  | length str == 25 && str !! 10 == ' ' = 
    case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%z" str of
      Just zonedTime -> zonedTime
      Nothing -> error ("Parsing for the 2nd case of time formats has failed: " ++ str)
  | str !! 10 == 'T' = 
    case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%f%z" str of
      Just zonedTime -> zonedTime
      Nothing -> error ("Parsing for the 3rd case of time formats has failed: " ++ str) 
  | str !! 10 == ' ' = 
    case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S.%f%z" str of
      Just zonedTime -> zonedTime
      Nothing -> error ("Parsing for the 4th case of time formats has failed: " ++ str)
  | otherwise = error ("Parsing has failed. The Faulty String value is: " ++ str)

