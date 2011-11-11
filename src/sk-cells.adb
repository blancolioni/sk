with Ada.Text_IO;

with SK.Images;

package body SK.Cells is

   Root_Register  : constant SK.Memory.Register := 0;
   Stack_Register : constant SK.Memory.Register := 1;
   Temp_Register  : constant SK.Memory.Register := 2;
   Count_Register : constant SK.Memory.Register := 3;

   First_User_Register : constant Natural := 4;

--     Current_Cells : Managed_Cells;

--     procedure Report_GC;

   function Can_Have_Child
     (Cells         : Managed_Cells;
      Parent, Child : Object)
      return Boolean;
   --  Return True if the parent object is allowed Child as its
   --  car or cdr.
   --  For example, a ROM object can't have children in RAM.


   function Get_Mem (Cells : Managed_Cells;
                     Item  : Object)
                    return Memory_Access;

   function Ram_Object (Cells : Managed_Cells;
                        X     : Object)
                        return Object;
   --  If X is already in RAM, then return it.  Otherwise, make a RAM
   --  copy of X, and return that.

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

   --------------------
   -- Can_Have_Child --
   --------------------

   function Can_Have_Child
     (Cells         : Managed_Cells;
      Parent, Child : Object)
      return Boolean
   is
   begin
      if not Is_Application (Parent)
        and then not Is_Lambda (Parent)
      then
         return False;
      end if;
      if not Is_Application (Child)
        and then not Is_Lambda (Child)
      then
         return True;
      end if;

      if Get_Mem (Cells, Parent) = Cells.Rom
        and then Get_Mem (Cells, Child) = Cells.Mem
      then
         Ada.Text_IO.Put_Line ("Bad parent/child "
                               & Hex_Image (Parent) & " "
                               & Hex_Image (Child));
         Ada.Text_IO.Put_Line ("Child = " & SK.Images.Image (Cells, Child));
         return False;
      end if;

      return True;

   end Can_Have_Child;

   ---------
   -- Car --
   ---------

   function Car
     (Cells : Managed_Cells;
      Item  : Object)
      return Object
   is
   begin
      return SK.Memory.Car (Get_Mem (Cells, Item).all,
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
      return SK.Memory.Cdr (Get_Mem (Cells, Item).all,
                            SK.Memory.To_Cell_Address (Item));
   end Cdr;

   --------------------------
   -- Create_Managed_Cells --
   --------------------------

   function Create_Managed_Cells
     (M, R : SK.Memory.Memory_Type)
      return Managed_Cells
   is
      Result : Managed_Cells;
      Root   : Object;
      Stack  : Object;
      X, Y   : Object := 0;
   begin
      Result.Mem := new SK.Memory.Memory_Type'(M);
      Result.Rom := new SK.Memory.Memory_Type'(R);
      Result.Next_Static := 0;
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

   -------------
   -- Get_Mem --
   -------------

   function Get_Mem (Cells : Managed_Cells;
                     Item  : Object)
                    return Memory_Access
   is
   begin
      if SK.Memory.In_Range (Cells.Mem.all, Item) then
         return Cells.Mem;
      elsif SK.Memory.In_Range (Cells.Rom.all, Item) then
         return Cells.Rom;
      else
         raise Constraint_Error with "bad address: " & Hex_Image (Item);
      end if;
   end Get_Mem;

   ---------
   -- Pop --
   ---------

   procedure Pop (Cells : in Managed_Cells) is
      SP : constant Object := Stack_Top_Cell (Cells);
   begin
      SK.Memory.Set_Register (Cells.Mem.all, Stack_Register, Cdr (Cells, SP));
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push
     (Cells : in Managed_Cells;
      Item  : in Object)
   is
      SP     : Object := Stack_Top_Cell (Cells);
      X      : Object := Ram_Object (Cells, Item);
      New_SP : Object;
   begin
      SK.Memory.Allocate (Cells.Mem.all,
                          Get_Bits (O_Application), X, SP, New_SP);
      SK.Memory.Set_Register (Cells.Mem.all, Stack_Register, New_SP);
   end Push;

   ----------------
   -- Ram_Object --
   ----------------

   function Ram_Object (Cells : Managed_Cells;
                        X     : Object)
                        return Object
   is
   begin
      if not Is_Application (X) then
         return X;
      elsif Get_Mem (Cells, X) /= Cells.Mem then
         declare
            Y   : Object;
            H   : Object := Car (Cells, X);
            T   : Object := Cdr (Cells, X);
         begin
            SK.Memory.Allocate (Cells.Mem.all,
                                Get_Bits (O_Application),
                                H, T, Y);
            Ada.Text_IO.Put_Line
              (Hex_Image (X) & " --> " & Hex_Image (Y));
            Ada.Text_IO.Put_Line (SK.Images.Image (Cells, Y));
            return Y;
         end;
      else
         return X;
      end if;
   end Ram_Object;

   ---------------
   -- Report_GC --
   ---------------

--     procedure Report_GC is
--        It : Object := SK.Memory.Get_Register (Current_Cells.Mem.all,
--                                               Stack_Register);
--     begin
--        Ada.Text_IO.Put_Line ("Got GC event");
--        while Is_Application (It) loop
--           Ada.Text_IO.Put_Line ("Stack: " &
--                                   SK.Images.Low_Level_Image
--                                   (Current_Cells,
--                                    Car (Current_Cells, It)));
--           It := Cdr (Current_Cells, It);
--        end loop;
--
--     end Report_GC;

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
      pragma Assert (Can_Have_Child (Cells, Item, To));
      SK.Memory.Set_Car (Get_Mem (Cells, Item).all,
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
      pragma Assert (Can_Have_Child (Cells, Item, To));
      SK.Memory.Set_Cdr (Get_Mem (Cells, Item).all,
                         SK.Memory.To_Cell_Address (Item), To);
   end Set_Cdr;

   --------------------
   -- Stack_Top_Cell --
   --------------------

   function Stack_Top_Cell (Cells : Managed_Cells) return Object is
   begin
      return SK.Memory.Get_Register (Cells.Mem.all, Stack_Register);
   end Stack_Top_Cell;

end SK.Cells;
