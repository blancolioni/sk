with SK.Cells;

private package SK.Images is

   function Image (Cells : SK.Cells.Managed_Cells;
                   Item  : Object)
                   return String;

   function Low_Level_Image (Cells : SK.Cells.Managed_Cells;
                             Item  : Object) return String;

   function Cell_Image (Cells : SK.Cells.Managed_Cells;
                        Item  : Object) return String;

end SK.Images;
