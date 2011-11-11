private with SK.Cells;

with SK.Machine;

package SK.Foreign is

   type Function_Call_Context is private;

   type Foreign_Function_Handler is access
     function (Context   : Function_Call_Context;
               Arguments : Array_Of_Objects)
               return Object;

   procedure Import_Function
     (Machine       : SK.Machine.SK_Machine;
      Function_Name : String;
      Arg_Count     : Natural;
      Handler       : Foreign_Function_Handler);

   generic
      type Foreign_Type is private;
      type Foreign_Pointer is access Foreign_Type;
   package Object_To_Access_Conversions is

   end Object_To_Access_Conversions;

private

   type Function_Call_Context is
     new SK.Cells.Managed_Cells;

end SK.Foreign;
