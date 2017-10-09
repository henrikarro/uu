structure Reversi_AI =
struct
  type T = unit
  val author = "Jean-Noel Monette"
  val nickname = "human"
  fun init _ = ()
  fun think (_, _, _) =
    let
      val _ = print "Please choose your next move (0...63 or return to skip): "
      val move = case Int.fromString (valOf (TextIO.inputLine TextIO.stdIn)) of
        SOME m => Move m
      | NONE => Pass
    in
      (move, ())
    end
end;
