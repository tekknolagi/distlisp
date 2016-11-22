-module(test).
-compile(export_all).

say_hi () ->
   receive {Node, hello} -> 
       Node ! {self(), hi}
   end.


check_node (Node)->
   Pid = spawn(Node, test, say_hi, []),
   Pid ! {self(), hello},
   receive {X, hi} -> io:format("~p~n", [X]) end.
 
