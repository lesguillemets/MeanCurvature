initials :: String -> String -> String
initials firstname surname = "Hello Mr. " ++ [f] ++ "." ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = surname
