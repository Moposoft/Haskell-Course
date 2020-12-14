								
  else if length args == 5 && (args !! 0) == "Tell" && (args !! 1) == "me" && (args !! 2) == "about"
  then do 
    putStrLn "I do not know of such event"
    loop $ return events
	else if length args == 4 && (args !! 0) == "What" && (args !! 1) == "happens" && (args !! 2) == "on"
  then do 
    putStrLn "Nothing that I know of"
    loop $ return events
  else if length args == 5 && (args !! 0) == "What" && (args !! 1) == "happens" && (args !! 2) == "at"
  then do 
    putStrLn "Nothing that I know of"
    loop $ return events
  else do
    putStrLn "I do not understand that. I understand the following:" 
    putStrLn "*Event <name> happens at <place> on <date>"
    putStrLn "*Tell me about <eventname>" 
    putStrLn "*What happens on <date>"
    putStrLn "*What happens at <place>" 
    putStrLn "*Quit"
    loop $ return events
 
  else if length args == 5 && (args !! 0) == "Tell" && (args !! 1) == "me" && (args !! 2) == "about"
  then do 
    putStrLn "I do not know of such event"
    loop $ return events
	else if length args == 4 && (args !! 0) == "What" && (args !! 1) == "happens" && (args !! 2) == "on"
  then do 
    putStrLn "Nothing that I know of"
    loop $ return events
  else if length args == 5 && (args !! 0) == "What" && (args !! 1) == "happens" && (args !! 2) == "at"
  then do 
    putStrLn "Nothing that I know of"
    loop $ return events
  else do
    putStrLn "I do not understand that. I understand the following:" 
    putStrLn "*Event <name> happens at <place> on <date>"
    putStrLn "*Tell me about <eventname>" 
    putStrLn "*What happens on <date>"
    putStrLn "*What happens at <place>" 
    putStrLn "*Quit"
    loop $ return events