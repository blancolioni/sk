with SK.Cells;

private package SK.Utilities is

    function String_To_List (Cells : SK.Cells.Managed_Cells;
                            Text  : String)
                            return Object;

  function To_List (Cells : SK.Cells.Managed_Cells;
                    Item  : Array_Of_Objects)
                    return Object;

end SK.Utilities;
