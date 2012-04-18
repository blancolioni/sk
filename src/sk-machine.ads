private with SK.Cells;

package SK.Machine is

   type SK_Machine is private;

   function Create_Machine (Size : Natural) return SK_Machine;
   function Load_Machine (Path : String) return SK_Machine;

   procedure Load_Library (M    : SK_Machine;
                           Path : String);

   function Start (M : SK_Machine) return Object;

   function Evaluate (M : SK_Machine;
                      E : Object)
                      return Object;

   procedure Evaluate (M : SK_Machine);
   --  Evaluates top of stack

   function Parse_String (M    : SK_Machine;
                          Text : String)
                          return Object;

   function Compile (M : SK_Machine;
                     E : Object)
                     return Object;

   function Show (M    : SK_Machine;
                  Item : Object)
                  return String;

   function Show_Stack_Top (M    : SK_Machine)
                           return String;

   procedure Apply
     (M    : SK_Machine);
   --  pop two objects off the top of the stack,
   --  push back the application of the two
   --  e.g. push f; push y; apply --> (f y)

   procedure Push
     (M    : SK_Machine;
      Item : SK.Object);

   procedure Bind (M         : SK_Machine;
                   Name      : String);
   --  bind object at top of stack to given name
   --  If Temporary is True, the definition is not moved to ROM

   type Function_Call_Context is private;

   type Foreign_Function_Handler is access
     function (Context   : Function_Call_Context;
               Arguments : SK.Array_Of_Objects)
               return SK.Object;

   procedure Import_Function
     (Machine       : SK_Machine;
      Function_Name : String;
      Arg_Count     : Natural;
      Handler       : Foreign_Function_Handler);

   generic
      type Foreign_Type is private;
      type Foreign_Pointer is access Foreign_Type;
   package Object_To_Access_Conversions is

   end Object_To_Access_Conversions;

   procedure Push (Context : Function_Call_Context;
                   Value   : Object);

   procedure Push (Context : Function_Call_Context;
                   Value   : Integer);

   procedure Push (Context   : Function_Call_Context;
                   Reference : String);

   procedure Pop (Context : Function_Call_Context;
                  Value   : out Object);

   function Car (Context : Function_Call_Context;
                 Value   : Object)
                 return Object;

   function Cdr (Context : Function_Call_Context;
                 Value   : Object)
                 return Object;

   function Marshall_String_To_Object
     (Context : Function_Call_Context;
      Value   : String)
      return Object;

   procedure Apply (Context : Function_Call_Context);

   procedure Compile (Context : Function_Call_Context);

   function Evaluate (Context : Function_Call_Context;
                      Value   : Object)
                      return Object;

   function Show (Context    : Function_Call_Context;
                  Item       : Object)
                  return String;

   function Show_Stack_Top (Context    : Function_Call_Context)
                           return String;

   function Low_Level_Show (M    : SK_Machine;
                            Item : Object)
                            return String;

   procedure Report_Memory (Machine : SK_Machine);

private

   type SK_Machine_Record;
   type SK_Machine is access SK_Machine_Record;

   function Get_Cells (M : SK_Machine) return SK.Cells.Managed_Cells;

   type Function_Call_Context is new SK.Cells.Managed_Cells;

end SK.Machine;
