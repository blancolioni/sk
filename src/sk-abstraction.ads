with SK.Cells;

private package SK.Abstraction is

   function Abstract_Expression (Cells : SK.Cells.Managed_Cells;
                                 E     : Object)
                                 return Object;

   procedure Optimise (Cells      : SK.Cells.Managed_Cells;
                       Expression : in out Object);

end SK.Abstraction;

