with SK.Cells;

package SK.Functions is

   type Evaluator is
     access function (Cells   : SK.Cells.Managed_Cells;
                      Args    : Array_Of_Objects)
                      return Object;
   --  Type signature for a function evaluator.

   function Get_Function_Name (Id : Function_Id) return String;

   procedure Evaluate (Id      : in     Function_Id;
                       Cells   : in     SK.Cells.Managed_Cells;
                       Result  :    out Object;
                       Changed :    out Boolean);

   procedure Bind_Function (Name     : String;
                            Args     : Natural;
                            Handler  : Evaluator);

   function Argument_Count (Id : Function_Id) return Natural;

private

   type Check_Function is
     access function (Item : Object) return Boolean;

   procedure Check (Item          : in Object;
                    Function_Name : in String;
                    Checker       : in Check_Function;
                    Message       : in String);

end SK.Functions;
