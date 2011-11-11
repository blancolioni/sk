with SK.Cells;

private package SK.Stack is

   procedure Push (Cells : in SK.Cells.Managed_Cells;
                   Item  : in Object);
   procedure Pop (Cells : in SK.Cells.Managed_Cells;
                  Item  : out Object);
   procedure Pop (Cells : in SK.Cells.Managed_Cells;
                  Items : out Array_Of_Objects);
   function Pop (Cells : SK.Cells.Managed_Cells) return Object;
   function Pop_All (Cells : SK.Cells.Managed_Cells) return Array_Of_Objects;
   procedure Drop (Cells : in SK.Cells.Managed_Cells;
                   Count : in Object);
   function Top (Cells : SK.Cells.Managed_Cells) return Object;
   procedure Top (Cells : in SK.Cells.Managed_Cells;
                  Items : out Array_Of_Objects);
   function Get (Cells : SK.Cells.Managed_Cells;
                 Index : Positive)
                 return Object;
   procedure Put (Cells : SK.Cells.Managed_Cells;
                  Index : Positive;
                  Value : Object);

   function Count (Cells : SK.Cells.Managed_Cells) return Natural;
   function Minimum_Count (Cells   : SK.Cells.Managed_Cells;
                           Minimum : Natural)
                           return Boolean;

   procedure Set_Boundary (Cells : SK.Cells.Managed_Cells);
   procedure Clear_Boundary (Cells : in SK.Cells.Managed_Cells);
   function Is_Empty (Cells : SK.Cells.Managed_Cells) return Boolean;

end SK.Stack;
