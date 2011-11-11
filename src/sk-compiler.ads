with SK.Cells;
with SK.Environments;

private package SK.Compiler is

   function Compile (Cells : SK.Cells.Managed_Cells;
                     Item  : Object)
                     return Object;

   function Link (Cells : SK.Cells.Managed_Cells;
                  Env   : SK.Environments.Environment;
                  Item  : Object)
                  return Object;
   --  Connect all object references in Item

end SK.Compiler;
