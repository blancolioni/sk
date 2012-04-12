package SK.Memory is

   type Memory_Type is private;

   type Cell_Address is mod 2**32;

   type Register is range 0 .. 15;

   function To_Cell_Address (Item : Object) return Cell_Address;

   function In_Range (Mem : Memory_Type;
                      Item : Object)
                     return Boolean;

   function Create_Managed_Memory (Cell_Count     : Cell_Address;
                                   Base_Address   : Cell_Address)
                                   return Memory_Type;
   --  Cell_Count: number of cells in this memory

   function Create_Extensible_Memory (Base_Address : Cell_Address)
                                     return Memory_Type;

   function Get_Cell_Count (From : Memory_Type) return Cell_Address;

   procedure Set_Register (M     : in Memory_Type;
                           R     : in Register;
                           Value : in Object);
   function Get_Register (M : Memory_Type;
                          R : Register)
                          return Object;

   function Car (M    : Memory_Type;
                 Addr : Cell_Address)
                 return Object;

   function Cdr (M    : Memory_Type;
                 Addr : Cell_Address)
                 return Object;

   procedure Set_Car (M    : Memory_Type;
                      Addr : Cell_Address;
                      To   : Object);

   procedure Set_Cdr (M    : Memory_Type;
                      Addr : Cell_Address;
                      To   : Object);

   procedure Copy_Structure (From    : in     Memory_Type;
                             To      : in out Memory_Type;
                             Start   : in out Object);

   procedure Copy_Cell (M    : in out Memory_Type;
                        Cell : in out Object;
                        Dest : in     Cell_Address);

   procedure Allocate (M             : in out Memory_Type;
                       Type_Bits     : in     Object;
                       X, Y          : in out Object;
                       Result        :    out Object);

   type GC_Callback is access procedure;

   procedure Set_GC_Callback (Callback : GC_Callback);

private

   type Cell_Array is array (Cell_Address range <>) of Object_Pair;
   type Cell_Array_Access is access Cell_Array;

   type Register_Array is array (Register) of Object;
   type Register_Array_Access is access Register_Array;

   type Memory_Type is
      record
         Cells        : Cell_Array_Access;
         Space_Size   : Cell_Address;
         To_Space     : Cell_Address;
         From_Space   : Cell_Address;
         Free         : Cell_Address;
         Top          : Cell_Address;
         Scan         : Cell_Address;
         R            : Register_Array_Access;
         Managed      : Boolean;
         Extensible   : Boolean;
      end record;

   pragma Inline (Car);
   pragma Inline (Cdr);
   pragma Inline (To_Cell_Address);
   pragma Inline (Get_Register);

end SK.Memory;
