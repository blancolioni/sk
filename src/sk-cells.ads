with SK.Memory;

package SK.Cells is

   type Managed_Cells is private;

   function Create_Managed_Cells
     (M : SK.Memory.Memory_Type)
     return Managed_Cells;

   function Root_Cell (Cells : Managed_Cells) return Object;
   function Stack_Top_Cell (Cells : Managed_Cells) return Object;

   procedure Push (Cells : in Managed_Cells;
                   Item  : in Object);
   function Top (Cells : Managed_Cells) return Object;
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

   procedure Pop (Cells : Managed_Cells;
                  Item  : out Object);

   procedure Apply (Cells : Managed_Cells);
   --  pop top two items from stack, apply them and push the result

   function Evaluate (Cells : Managed_Cells;
                      Item  : Object)
                      return Object;

   procedure Compile (Context : Managed_Cells);

   function Marshall_String_To_Object
     (Context : Managed_Cells;
      Value   : String)
      return Object;

   function Marshall_Array_To_List
     (Context : Managed_Cells;
      Value   : Array_Of_Objects)
      return Object;

   function Marshall_List_To_Array
     (Cells : Managed_Cells;
      Value : SK.Object)
      return Array_Of_Objects;

   procedure Report_Memory (Cells : Managed_Cells);

   function Image (Cells : Managed_Cells;
                   Item  : Object)
                   return String;

private

   type Memory_Access is access SK.Memory.Memory_Type;

   type Managed_Cells is
      record
         Mem         : Memory_Access;
      end record;

end SK.Cells;
