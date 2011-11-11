with SK.Cells;

private package SK.Combinators is

   function Evaluate (Cells : SK.Cells.Managed_Cells;
                      Comb  : Combinator)
                      return Object;

   --  Evaluate the given combinator by re-arranging application nodes.
   --  Each element of Args should be an application of the previous
   --  argument to the next.  The first argument applies the combinator
   --  (though this fact is not used).
   --  The top of the new tree is returned.

end SK.Combinators;
