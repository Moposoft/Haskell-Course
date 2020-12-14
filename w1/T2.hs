myFunction :: [String] -> Char -> [String]
myFunction list char = [ string | string <- list, last(string)==char || head(string)==char]