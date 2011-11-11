with SK.Stacks;

private package SK.Allocation is

   function Allocate (Class : Object) return Object;

   function Car (Item : Object) return Object;
   function Cdr (Item : Object) return Object;

   procedure Set_Car (Item : Object;
                      To   : Object);
   procedure Set_Cdr (Item : Object;
                      To   : Object);

   procedure Protect (Item : Object);
   procedure Unprotect (Item : Object);

   procedure Collect;
   procedure Collect (Watch : in out Object);
   --  Collect garbage, and update Watch to its new value, if any

   procedure Disable_GC;
   procedure Enable_GC;

   function New_Stack return SK.Stacks.Evaluation_Stack;

   function Get_Max_Allocated_Pairs return Object;
   function Get_Number_Of_Collections return Natural;

   function Mark_Object (Top : Object) return Object;

   procedure Import_Image (Image : Array_Of_Objects);
   function  Export_Image return Array_Of_Objects;

   function Get_Pair (Item : Object) return Object_Pair;
   procedure Set_Pair (Item : Object;
                       To   : Object_Pair);

private

   pragma Inline (Car);
   pragma Inline (Cdr);

   pragma Inline (Set_Car);
   pragma Inline (Set_Cdr);

   pragma Inline (Allocate);

end SK.Allocation;
