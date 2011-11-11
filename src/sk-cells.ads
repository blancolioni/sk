with SK.Memory;

private package SK.Cells is

   type Managed_Cells is private;

   function Create_Managed_Cells
     (M, R : SK.Memory.Memory_Type)
     return Managed_Cells;

   function Root_Cell (Cells : Managed_Cells) return Object;
   function Stack_Top_Cell (Cells : Managed_Cells) return Object;

   procedure Push (Cells : in Managed_Cells;
                   Item  : in Object);
   procedure Pop (Cells : in Managed_Cells);

   function Car (Cells : Managed_Cells;
                 Item  : Object)
                 return Object;

   function Cdr (Cells : Managed_Cells;
                 Item  : Object)
                 return Object;

   procedure Set_Car (Cells : in Managed_Cells;
                      Item  : in Object;
                      To    : in Object);

   procedure Set_Cdr (Cells : in Managed_Cells;
                      Item  : in Object;
                      To    : in Object);

   procedure Allocate (Cells  : in     Managed_Cells;
                       Class  : in     Object_Class;
                       Result :    out Array_Of_Objects);

   procedure Allocate (Cells  : in     Managed_Cells;
                       Class  : in     Object_Class;
                       Result :    out Object);

private

   type Memory_Access is access SK.Memory.Memory_Type;

   type Managed_Cells is
      record
         Mem         : Memory_Access;
         Rom         : Memory_Access;
         Next_Static : SK.Memory.Cell_Address;
      end record;

end SK.Cells;
