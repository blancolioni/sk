with SK.Compiler;
with SK.Environments;
with SK.Evaluator;

package body SK.Cells is

   Root_Register  : constant SK.Memory.Register := 0;
   Stack_Register : constant SK.Memory.Register := 1;
   Temp_Register  : constant SK.Memory.Register := 2;
   Count_Register : constant SK.Memory.Register := 3;

   First_User_Register : constant Natural := 4;

   --------------
   -- Allocate --
   --------------

   procedure Allocate (Cells  : in     Managed_Cells;
                       Class  : in     Object_Class;
                       Result :    out Object)
   is
      A : Array_Of_Objects (1 .. 1);
   begin
      Allocate (Cells, Class, A);
      Result := A (A'First);
   end Allocate;

   --------------
   -- Allocate --
   --------------

   procedure Allocate (Cells  : in    Managed_Cells;
                       Class  : in    Object_Class;
                       Result :    out Array_Of_Objects)
   is
      T0, T1 : Object := Null_Object;
   begin
--        Current_Cells := Cells;
      for I in Result'Range loop
         SK.Memory.Allocate (Cells.Mem.all, Get_Bits (Class), T0, T0, T1);
         SK.Memory.Set_Register (Cells.Mem.all, Temp_Register, T1);
         T0 := T1;
      end loop;
      T0 := SK.Memory.Get_Register (Cells.Mem.all, Temp_Register);
      for I in Result'Range loop
         Result (I) := T0;
         T0 := SK.Memory.Car (Cells.Mem.all, SK.Memory.To_Cell_Address (T0));
      end loop;
   end Allocate;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Cells : Managed_Cells)
   is
      Result, Left, Right : Object;
   begin
      Allocate (Cells, O_Application, Result);
      Pop (Cells, Left);
      Pop (Cells, Right);
      Set_Car (Cells, Result, Left);
      Set_Cdr (Cells, Result, Right);
      Push (Cells, Result);
   end Apply;

   ---------
   -- Car --
   ---------

   function Car
     (Cells : Managed_Cells;
      Item  : Object)
      return Object
   is
   begin
      return SK.Memory.Car (Cells.Mem.all,
                            SK.Memory.To_Cell_Address (Item));
   end Car;

   ---------
   -- Cdr --
   ---------

   function Cdr
     (Cells : Managed_Cells;
      Item  : Object)
      return Object
   is
   begin
      return SK.Memory.Cdr (Cells.Mem.all,
                            SK.Memory.To_Cell_Address (Item));
   end Cdr;

   -------------
   -- Compile --
   -------------

   procedure Compile (Context : Managed_Cells) is
      E      : Object;
      Result : Object;
   begin
      Pop (Context, E);
      Result := SK.Compiler.Compile (Context, E);
      Result := SK.Compiler.Link
        (Context, SK.Environments.Top_Level_Environment, Result);
      Push (Context, Result);
   end Compile;

   --------------------------
   -- Create_Managed_Cells --
   --------------------------

   function Create_Managed_Cells
     (M : SK.Memory.Memory_Type)
      return Managed_Cells
   is
      Result : Managed_Cells;
      Root   : Object;
      Stack  : Object;
      X, Y   : Object := 0;
   begin
      Result.Mem := new SK.Memory.Memory_Type'(M);
      SK.Memory.Allocate (Result.Mem.all,
                          Get_Bits (O_Application), X, Y, Root);
      SK.Memory.Set_Register (Result.Mem.all, Root_Register, Root);
      SK.Memory.Allocate (Result.Mem.all,
                          Get_Bits (O_Application), X, Y, Stack);
      SK.Memory.Set_Register (Result.Mem.all, Stack_Register, Stack);

      SK.Memory.Set_Register (Result.Mem.all, Count_Register,
                              To_Object (First_User_Register));

      return Result;
   end Create_Managed_Cells;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Cells : Managed_Cells;
                      Item  : Object)
                      return Object
   is
   begin
      return SK.Evaluator.Evaluate (Cells, Item);
   end Evaluate;

   -------------------------------
   -- Marshall_String_To_Object --
   -------------------------------

   function Marshall_String_To_Object
     (Context : Managed_Cells;
      Value   : String)
      return Object
   is
   begin
      Push (Context, Get_Symbol ("[]"));
      for I in reverse Value'Range loop
         Push (Context, To_Object (Integer'(Character'Pos (Value (I)))));
         Push (Context, Get_Symbol (":"));
         Apply (Context);
         Apply (Context);
      end loop;

      Compile (Context);

      declare
         Result : Object;
      begin
         Pop (Context, Result);
         return Result;
      end;

   end Marshall_String_To_Object;

   ---------
   -- Pop --
   ---------

   procedure Pop (Cells : in Managed_Cells) is
      SP : constant Object := Stack_Top_Cell (Cells);
   begin
      SK.Memory.Set_Register (Cells.Mem.all, Stack_Register, Cdr (Cells, SP));
   end Pop;

   ---------
   -- Pop --
   ---------

   procedure Pop (Cells : Managed_Cells;
                  Item  : out Object)
   is
   begin
      Item := Top (Cells);
      Pop (Cells);
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push
     (Cells : in Managed_Cells;
      Item  : in Object)
   is
      SP     : Object := Stack_Top_Cell (Cells);
      X      : Object := Item;
      New_SP : Object;
   begin
      SK.Memory.Allocate (Cells.Mem.all,
                          Get_Bits (O_Application), X, SP, New_SP);
      SK.Memory.Set_Register (Cells.Mem.all, Stack_Register, New_SP);
   end Push;

   ---------------
   -- Root_Cell --
   ---------------

   function Root_Cell (Cells : Managed_Cells) return Object is
   begin
      return SK.Memory.Get_Register (Cells.Mem.all, 0);
   end Root_Cell;

   -------------
   -- Set_Car --
   -------------

   procedure Set_Car
     (Cells : in Managed_Cells;
      Item  : in Object;
      To    : in Object)
   is
   begin
      SK.Memory.Set_Car (Cells.Mem.all,
                         SK.Memory.To_Cell_Address (Item), To);
   end Set_Car;

   -------------
   -- Set_Cdr --
   -------------

   procedure Set_Cdr
     (Cells : in Managed_Cells;
      Item  : in Object;
      To    : in Object)
   is
   begin
      SK.Memory.Set_Cdr (Cells.Mem.all,
                         SK.Memory.To_Cell_Address (Item), To);
   end Set_Cdr;

   --------------------
   -- Stack_Top_Cell --
   --------------------

   function Stack_Top_Cell (Cells : Managed_Cells) return Object is
   begin
      return SK.Memory.Get_Register (Cells.Mem.all, Stack_Register);
   end Stack_Top_Cell;

   ---------
   -- Top --
   ---------

   function Top (Cells : Managed_Cells) return Object is
   begin
      return Car (Cells, Stack_Top_Cell (Cells));
   end Top;

end SK.Cells;
