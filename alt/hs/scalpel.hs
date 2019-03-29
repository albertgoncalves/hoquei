{-# OPTIONS_GHC -Wall #-}

import Text.ParserCombinators.ReadP

scalpel :: ReadP String
scalpel = skip '>' >> parse >>= (\x -> skip '<' >> return x)
  where
    skip = skipMany1 . satisfy . (==)
    parse = many1 $ satisfy (`notElem` "><")

extract :: [(String, a)] -> [String]
extract [(s, _)] = [s]
extract _ = []

surgery :: String -> [String]
surgery =
    concatMap (extract . readP_to_S scalpel)
    . takeWhile (/= [])
    . iterate tail

excerpt :: String
excerpt =
    "<tr ><th scope=\"row\" class=\"left \" data-stat=\"date_game\" csk=\"201810030SJS\" ><a href=\"/boxscores/201810030SJS.html\">2018-10-03</a></th><td class=\"left \" data-stat=\"visitor_team_name\" csk=\"ANA.201810030SJS\" ><a href=\"/teams/ANA/2019.html\">Anaheim Ducks</a></td><td class=\"right \" data-stat=\"visitor_goals\" >5</td><td class=\"left \" data-stat=\"home_team_name\" csk=\"SJS.201810030SJS\" ><a href=\"/teams/SJS/2019.html\">San Jose Sharks</a></td><td class=\"right \" data-stat=\"home_goals\" >2</td><td class=\"center iz\" data-stat=\"overtimes\" csk=\"0\" ></td><td class=\"right \" data-stat=\"attendance\" >17,562</td><td class=\"right \" data-stat=\"game_duration\" >2:25</td><td class=\"left iz\" data-stat=\"game_remarks\" ></td></tr>"

main :: IO ()
main = print $ surgery excerpt
