with SK.Cells;

private package SK.Parser is

   function Parse_String (Cells : SK.Cells.Managed_Cells;
                          S     : String)
                          return Object;

end SK.Parser;
