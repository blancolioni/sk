with Ada.Unchecked_Conversion;

with SK.Compiler;
with SK.Environments;
with SK.Evaluator;
with SK.Functions.Primitives;
with SK.Images;
with SK.Memory;
with SK.Parser;
with SK.Stack;

package body SK.Machine is

   type SK_Machine_Record is
      record
         Cells  : SK.Cells.Managed_Cells;
         Env    : SK.Environments.Environment;
         Start  : Object;
      end record;

   procedure Define_Primitive (M     : SK_Machine;
                               Name  : String;
                               Code  : String);

   -----------
   -- Apply --
   -----------

   procedure Apply
     (M    : SK_Machine)
   is
      Result, Left, Right : Object;
   begin
      SK.Cells.Allocate (M.Cells, O_Application, Result);
      SK.Stack.Pop (M.Cells, Right);
      SK.Stack.Pop (M.Cells, Left);
      SK.Cells.Set_Car (M.Cells, Result, Left);
      SK.Cells.Set_Cdr (M.Cells, Result, Right);
      SK.Stack.Push (M.Cells, Result);
   end Apply;

   -----------
   -- Apply --
   -----------

   overriding procedure Apply
     (Context : Function_Call_Context)
   is
      Cells  : constant SK.Cells.Managed_Cells :=
                 SK.Cells.Managed_Cells (Context);
      Result, Left, Right : Object;
   begin
      SK.Cells.Allocate (Cells, O_Application, Result);
      SK.Stack.Pop (Cells, Left);
      SK.Stack.Pop (Cells, Right);
      SK.Cells.Set_Car (Cells, Result, Left);
      SK.Cells.Set_Cdr (Cells, Result, Right);
      SK.Stack.Push (Cells, Result);
   end Apply;

   ----------
   -- Bind --
   ----------

   procedure Bind (M         : SK_Machine;
                   Name      : String)
   is
      E : Object := SK.Stack.Top (M.Cells);
   begin
      E := SK.Compiler.Compile (M.Cells, E);
      SK.Stack.Push (M.Cells, E);

      SK.Stack.Drop (M.Cells, 2);

      SK.Environments.Define (M.Env, Name, E);
   end Bind;

   ---------
   -- Car --
   ---------

   overriding function Car (Context : Function_Call_Context;
                 Value   : Object)
                 return Object
   is
   begin
      return SK.Cells.Car (SK.Cells.Managed_Cells (Context), Value);
   end Car;

   ---------
   -- Cdr --
   ---------

   overriding function Cdr (Context : Function_Call_Context;
                 Value   : Object)
                 return Object
   is
   begin
      return SK.Cells.Cdr (SK.Cells.Managed_Cells (Context), Value);
   end Cdr;

   -------------
   -- Compile --
   -------------

   function Compile (M : SK_Machine;
                     E : Object)
                     return Object
   is
      Result : constant Object := SK.Compiler.Compile (M.Cells, E);
   begin
      return SK.Compiler.Link (M.Cells, M.Env, Result);
   end Compile;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile (Context : Function_Call_Context) is
      Cells  : constant SK.Cells.Managed_Cells :=
                 SK.Cells.Managed_Cells (Context);
      E      : Object;
      Result : Object;
   begin
      Pop (Context, E);
      Result := SK.Compiler.Compile (Cells, E);
      Result := SK.Compiler.Link
        (Cells, SK.Environments.Top_Level_Environment, Result);
      Push (Context, Result);
   end Compile;

   --------------------
   -- Create_Machine --
   --------------------

   function Create_Machine (Size : Natural) return SK_Machine is
      Mem    : constant SK.Memory.Memory_Type :=
                 SK.Memory.Create_Managed_Memory
                   (Cell_Count     => SK.Memory.Cell_Address (Size),
                    Base_Address   => 0);
      Result : constant SK_Machine := new SK_Machine_Record'
        (Cells => SK.Cells.Create_Managed_Cells (Mem),
         Env   => SK.Environments.Top_Level_Environment,
         Start => 0);
   begin
      SK.Functions.Primitives.Add_Primitives;

--        Define_Primitive (Result, "true", "\x.\y.y");
--        Define_Primitive (Result, "false", "\x.\y.x");
--        Define_Primitive (Result, "if", "\p.\f.\t.p f t");
      Define_Primitive (Result, "Y", "\f.(\x.f (x x)) (\x.f (x x))");
--        Define_Primitive (Result, "cons", "\h.\t.\x.\y.y h t");
      Define_Primitive (Result, "Y'",
                        "S (K (S I I)) (S (S (K S) K) (K (S I I)))");
      return Result;
   end Create_Machine;

   ----------------------
   -- Define_Primitive --
   ----------------------

   procedure Define_Primitive (M     : SK_Machine;
                               Name  : String;
                               Code  : String)
   is
      E : Object;
   begin
      E := SK.Parser.Parse_String (M.Cells, Code);
      SK.Stack.Push (M.Cells, E);
      E := SK.Compiler.Compile (M.Cells, E);
      SK.Stack.Push (M.Cells, E);
      E := SK.Compiler.Link (M.Cells, M.Env, E);
      SK.Stack.Push (M.Cells, E);

      SK.Environments.Define (M.Env, Name, E);

      SK.Stack.Drop (M.Cells, 3);

   end Define_Primitive;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (M : SK_Machine;
      E : Object)
      return Object
   is
      Value : Object := E;
   begin
      SK.Evaluator.Evaluate (M.Cells, Value);
      return Value;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate (M : SK_Machine) is
      V : Object := SK.Stack.Top (M.Cells);
   begin
      V := Compile (M, V);
      SK.Evaluator.Evaluate (M.Cells, V);
      SK.Stack.Drop (M.Cells, 1);
      SK.Stack.Push (M.Cells, V);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate (Context : Function_Call_Context;
                      Value   : Object)
                      return Object
   is
      E : Object := Value;
   begin
      SK.Evaluator.Evaluate (SK.Cells.Managed_Cells (Context), E);
      return E;
   end Evaluate;

   ---------------
   -- Get_Cells --
   ---------------

   function Get_Cells (M : SK_Machine) return SK.Cells.Managed_Cells is
   begin
      return M.Cells;
   end Get_Cells;

   ---------------------
   -- Import_Function --
   ---------------------

--     procedure Import_Function
--       (Machine       : SK_Machine;
--        Function_Name : String;
--        Arg_Count     : Natural;
--        Handler       : Foreign_Function_Handler)
--     is
--     begin
--        SK.Functions.Add_External_Function
--          (Env     => Machine.Env,
--           Name    => Function_Name,
--           Args    => Arg_Count,
--           Handler => SK.Functions.Evaluator (Handler),
--           Strict  => True);
--     end Import_Function;

   ---------------------
   -- Import_Function --
   ---------------------

   procedure Import_Function
     (Machine       : SK_Machine;
      Function_Name : String;
      Arg_Count     : Natural;
      Handler       : Foreign_Function_Handler)
   is
      pragma Unreferenced (Machine);
      function To_Evaluator is
        new Ada.Unchecked_Conversion (Foreign_Function_Handler,
                                      SK.Functions.Evaluator);
   begin
      SK.Functions.Bind_Function
        (Name    => Function_Name,
         Args    => Arg_Count,
         Handler => To_Evaluator (Handler));
   end Import_Function;

   ------------------
   -- Load_Library --
   ------------------

   procedure Load_Library
     (M    : SK_Machine;
      Path : String)
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
   end Load_Library;

   ------------------
   -- Load_Machine --
   ------------------

   function Load_Machine (Path : String) return SK_Machine is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return Load_Machine (Path);
   end Load_Machine;

   --------------------
   -- Low_Level_Show --
   --------------------

   function Low_Level_Show (M    : SK_Machine;
                            Item : Object)
                            return String
   is
   begin
      return SK.Images.Low_Level_Image (M.Cells, Item);
   end Low_Level_Show;

   -------------------------------
   -- Marshall_String_To_Object --
   -------------------------------

   overriding function Marshall_String_To_Object
     (Context : Function_Call_Context;
      Value   : String)
      return Object
   is
   begin
      Push (Context, "[]");
      for I in reverse Value'Range loop
         Push (Context, Integer'(Character'Pos (Value (I))));
         Push (Context, ":");
         Apply (Context);
         Apply (Context);
      end loop;

      --  Ada.Text_IO.Put_Line (SK.Machine.Show_Stack_Top (Context));
      Compile (Context);

      declare
         Result : Object;
      begin
         Pop (Context, Result);
         return Result;
      end;

   end Marshall_String_To_Object;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String (M    : SK_Machine;
                          Text : String)
                          return Object
   is
   begin
      return SK.Parser.Parse_String (M.Cells, Text);
   end Parse_String;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop (Context : Function_Call_Context;
                  Value   : out Object)
   is
   begin
      SK.Stack.Pop (SK.Cells.Managed_Cells (Context), Value);
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push
     (M    : SK_Machine;
      Item : SK.Object)
   is
   begin
      SK.Stack.Push (M.Cells, Item);
   end Push;

   ----------
   -- Push --
   ----------

   overriding procedure Push (Context : Function_Call_Context;
                   Value   : Object)
   is
   begin
      SK.Stack.Push (SK.Cells.Managed_Cells (Context), Value);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (Context : Function_Call_Context;
                   Value   : Integer)
   is
   begin
      Push (Context, To_Object (Value));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (Context   : Function_Call_Context;
                   Reference : String)
   is
   begin
      Push (Context, Get_Symbol (Reference));
   end Push;

   -------------------
   -- Report_Memory --
   -------------------

   procedure Report_Memory (Machine : SK_Machine) is
   begin
      SK.Cells.Report_Memory (Machine.Cells);
   end Report_Memory;

   ----------
   -- Show --
   ----------

   function Show (M    : SK_Machine;
                  Item : Object)
                  return String
   is
   begin
      return SK.Images.Image (M.Cells, Item);
   end Show;

   ----------
   -- Show --
   ----------

   function Show (Context    : Function_Call_Context;
                  Item       : Object)
                  return String
   is
   begin
      return SK.Images.Image (SK.Cells.Managed_Cells (Context), Item);
   end Show;

   --------------------
   -- Show_Stack_Top --
   --------------------

   function Show_Stack_Top (M    : SK_Machine)
                           return String
   is
   begin
      return Show (M, SK.Stack.Top (M.Cells));
   end Show_Stack_Top;

   --------------------
   -- Show_Stack_Top --
   --------------------

   function Show_Stack_Top (Context : Function_Call_Context)
                           return String
   is
   begin
      return SK.Images.Image
        (SK.Cells.Managed_Cells (Context),
         SK.Stack.Top (SK.Cells.Managed_Cells (Context)));
   end Show_Stack_Top;

   -----------
   -- Start --
   -----------

   function Start (M : SK_Machine) return Object is
   begin
      return M.Start;
   end Start;

end SK.Machine;
