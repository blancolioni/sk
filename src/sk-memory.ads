with SK.Objects;

package SK.Memory is

   type SK_Memory is tagged limited private;

   function Valid (Memory  : SK_Memory;
                   Address : SK.Objects.Cell_Address)
                   return Boolean;

   procedure Get
     (Memory    : SK_Memory;
      Address   : SK.Objects.Cell_Address;
      Car, Cdr  : out SK.Objects.Object)
     with Pre => Valid (Memory, Address);

   function Car (Memory : SK_Memory;
                 Address : SK.Objects.Cell_Address)
                 return SK.Objects.Object
     with Pre => Valid (Memory, Address);

   function Cdr (Memory : SK_Memory;
                 Address : SK.Objects.Cell_Address)
                 return SK.Objects.Object
     with Pre => Valid (Memory, Address);

   procedure Set
     (Memory            : in out SK_Memory;
      Address           : SK.Objects.Cell_Address;
      New_Car, New_Cdr  : SK.Objects.Object)
     with Pre => Valid (Memory, Address),
     Post => SK.Objects."=" (Car (Memory, Address), New_Car)
     and then SK.Objects."=" (Cdr (Memory, Address), New_Cdr);

   procedure Set_Car
     (Memory  : in out SK_Memory;
      Address : SK.Objects.Cell_Address;
      New_Car : SK.Objects.Object)
     with Pre => Valid (Memory, Address),
     Post => SK.Objects."=" (Car (Memory, Address), New_Car);

   procedure Set_Cdr
     (Memory  : in out SK_Memory;
      Address : SK.Objects.Cell_Address;
      New_Cdr : SK.Objects.Object)
     with Pre => Valid (Memory, Address),
     Post => SK.Objects."=" (Cdr (Memory, Address), New_Cdr);

   function Allocate
     (Memory    : in out SK_Memory;
      Car, Cdr  : SK.Objects.Object)
      return SK.Objects.Object
     with Post => SK.Objects.Is_Application (Allocate'Result)
     and then Valid (Memory, SK.Objects.Get_Address (Allocate'Result));

   type GC_Interface is limited interface;

   procedure Before_GC (GC : in out GC_Interface) is null;
   procedure After_GC (GC : in out GC_Interface) is null;
   procedure Mark_External_Object
     (GC       : in out GC_Interface;
      External : SK.Objects.External_Object_Id;
      Mark     : not null access
        procedure (X : in out SK.Objects.Object))
   is null;

   type GC_Callback is access all GC_Interface'Class;

   procedure Create
     (Memory    : in out SK_Memory'class;
      Core_Size : SK.Objects.Cell_Address;
      Callback  : GC_Callback);

   procedure Report
     (Memory : SK_Memory);

   procedure Mark (Memory : in out SK_Memory;
                   Item   : in out SK.Objects.Object);

   procedure Reserve_Memory
     (Memory  : in out SK_Memory;
      Minimum : Natural);

private

   type Object_Pair is
      record
         Car, Cdr : SK.Objects.Object;
      end record
     with Pack, Size => 64;

   type Core_Memory_Type is
     array (SK.Objects.Cell_Address range <>) of Object_Pair;

   type Core_Memory_Access is access all Core_Memory_Type;

   type SK_Memory is tagged limited
      record
         Core        : Core_Memory_Access;
         Top         : SK.Objects.Cell_Address;
         Free        : SK.Objects.Cell_Address;
         From_Space  : SK.Objects.Cell_Address;
         To_Space    : SK.Objects.Cell_Address;
         Space_Size  : SK.Objects.Cell_Address;
         Scan        : SK.Objects.Cell_Address;
         Callback    : GC_Callback;
         Alloc_Car   : SK.Objects.Object;
         Alloc_Cdr   : SK.Objects.Object;
         Test        : SK.Objects.Object;
         Alloc_Count : Natural;
         GC_Count    : Natural  := 0;
         GC_Time     : Duration := 0.0;
         Allocations : Natural := 0;
         Collections : Natural := 0;
      end record;

end SK.Memory;
