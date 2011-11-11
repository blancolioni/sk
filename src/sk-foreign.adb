with Ada.Containers.Vectors;

with SK.Functions;

package body SK.Foreign is

   type Foreign_Function_Record is
      record
         Handler   : Foreign_Function_Handler;
      end record;

   package Foreign_Function_Vectors is
     new Ada.Containers.Vectors
       (Positive, Foreign_Function_Record);

   Foreign_Functions : Foreign_Function_Vectors.Vector;

   function Evaluate_Foreign_Function
     (Cells   : SK.Cells.Managed_Cells;
      Args    : Array_Of_Objects)
      return Object;

   ---------------------
   -- Import_Function --
   ---------------------

   procedure Import_Function
     (Machine       : SK.Machine.SK_Machine;
      Function_Name : String;
      Arg_Count     : Natural;
      Handler       : Foreign_Function_Handler)
   is
   begin

   end Import_Function;

end SK.Foreign;
