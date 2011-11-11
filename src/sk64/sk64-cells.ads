package SK64.Cells is

   function Allocate (Class   : Object_Type;
                      Count   : Positive)
                     return Object;
   --  Allocates @Count@ pairs.  The return value points to the
   --  first pair.  The other pairs, if any, are R+4, R+8, ...
   --  where R is the function result

end SK64.Cells;
