with SK.Cells;
with SK.Environments;

private package SK.Functions is

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

   procedure Add_External_Function (Env      : SK.Environments.Environment;
                                    Name     : String;
                                    Args     : Natural;
                                    Handler  : Evaluator;
                                    Strict   : Boolean);

private

   type Check_Function is
     access function (Item : Object) return Boolean;

   procedure Check (Cells         : in SK.Cells.Managed_Cells;
                    Item          : in Object;
                    Function_Name : in String;
                    Checker       : in Check_Function;
                    Message       : in String);

   type Lazy_Evaluator is
     access function (Cells : SK.Cells.Managed_Cells)
                     return Object;

   procedure Add_Function (Env            : SK.Environments.Environment;
                           Name           : String;
                           Args           : Natural;
                           Handler        : Lazy_Evaluator;
                           Strict_Handler : Evaluator;
                           Internal       : Boolean;
                           Strict         : Boolean;
                           Lazy_Result    : Boolean   := True);

   procedure Add_Internal_Function (Env         : SK.Environments.Environment;
                                    Name        : String;
                                    Args        : Natural;
                                    Handler     : Lazy_Evaluator;
                                    Lazy_Result : Boolean    := True);

   function Ground_Argument_Index (Cells : SK.Cells.Managed_Cells;
                                   Index    : Positive)
                                   return Object;

end SK.Functions;
