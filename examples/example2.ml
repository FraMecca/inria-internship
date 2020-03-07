external observe : 'a -> 'b = "observe";;

let r = function
  | 2 -> observe 2
  | 1 -> observe 1
  |_ -> observe 0;;
