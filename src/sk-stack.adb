package body SK.Stack is

   --------------------
   -- Clear_Boundary --
   --------------------

   procedure Clear_Boundary (Cells : in SK.Cells.Managed_Cells) is
   begin
      pragma Assert (Is_Empty (Cells));
      SK.Cells.Pop (Cells);
   end Clear_Boundary;

   -----------
   -- Count --
   -----------

   function Count (Cells : SK.Cells.Managed_Cells) return Natural is
      It     : Object := SK.Cells.Stack_Top_Cell (Cells);
      Result : Natural := 0;
   begin
      while SK.Cells.Car (Cells, It) /= Boundary_Object loop
         if not Is_Application (It) then
            raise Constraint_Error with "ran out of stack";
         end if;

         It := SK.Cells.Cdr (Cells, It);
         Result := Result + 1;
      end loop;
      return Result;
   end Count;

   ----------
   -- Drop --
   ----------

   procedure Drop
     (Cells : in SK.Cells.Managed_Cells;
      Count : in Object)
   is
   begin
      for I in 1 .. Count loop
         if Top (Cells) = Boundary_Object then
            raise Constraint_Error with "crossed stack boundary";
         end if;

         SK.Cells.Pop (Cells);
      end loop;
   end Drop;

   ---------
   -- Get --
   ---------

   function Get
     (Cells : SK.Cells.Managed_Cells;
      Index : Positive)
      return Object
   is
      Items : Array_Of_Objects (1 .. Index);
   begin
      Top (Cells, Items);
      return Items (Items'Last);
   end Get;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Cells : SK.Cells.Managed_Cells) return Boolean is
   begin
      return Top (Cells) = Boundary_Object;
   end Is_Empty;

   -------------------
   -- Minimum_Count --
   -------------------

   function Minimum_Count
     (Cells   : SK.Cells.Managed_Cells;
      Minimum : Natural)
      return Boolean
   is
      It    : Object := SK.Cells.Stack_Top_Cell (Cells);
   begin
      for I in 1 .. Minimum loop
         if SK.Cells.Car (Cells, It) = Boundary_Object then
            return False;
         end if;
         It := SK.Cells.Cdr (Cells, It);
      end loop;
      return True;
   end Minimum_Count;

   ---------
   -- Pop --
   ---------

   procedure Pop
     (Cells : in     SK.Cells.Managed_Cells;
      Item  :    out Object)
   is
   begin
      Item := Top (Cells);
      if Top (Cells) = Boundary_Object then
         raise Constraint_Error with "crossed stack boundary";
      end if;

      SK.Cells.Pop (Cells);
   end Pop;

   ---------
   -- Pop --
   ---------

   procedure Pop
     (Cells : in SK.Cells.Managed_Cells;
      Items : out Array_Of_Objects)
   is
   begin
      for I in Items'Range loop
         Pop (Cells, Items (I));
      end loop;
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop (Cells : SK.Cells.Managed_Cells) return Object is
      Result : Object;
   begin
      Pop (Cells, Result);
      return Result;
   end Pop;

   -------------
   -- Pop_All --
   -------------

   function Pop_All
     (Cells : SK.Cells.Managed_Cells)
      return Array_Of_Objects
   is
      Result : Array_Of_Objects (1 .. Count (Cells));
   begin
      Pop (Cells, Result);
      return Result;
   end Pop_All;

   ----------
   -- Push --
   ----------

   procedure Push
     (Cells : in SK.Cells.Managed_Cells;
      Item  : in Object)
   is
   begin
      SK.Cells.Push (Cells, Item);
   end Push;

   ---------
   -- Put --
   ---------

   procedure Put
     (Cells : SK.Cells.Managed_Cells;
      Index : Positive;
      Value : Object)
   is
      It : Object := SK.Cells.Stack_Top_Cell (Cells);
   begin
      for I in 1 .. Index - 1 loop
         It := SK.Cells.Cdr (Cells, It);
      end loop;
      SK.Cells.Set_Car (Cells, It, Value);
   end Put;

   ------------------
   -- Set_Boundary --
   ------------------

   procedure Set_Boundary (Cells : SK.Cells.Managed_Cells) is
   begin
      Push (Cells, Boundary_Object);
   end Set_Boundary;

   ---------
   -- Top --
   ---------

   function Top (Cells : SK.Cells.Managed_Cells) return Object is
   begin
      return SK.Cells.Car (Cells, SK.Cells.Stack_Top_Cell (Cells));
   end Top;

   ---------
   -- Top --
   ---------

   procedure Top
     (Cells : in SK.Cells.Managed_Cells;
      Items : out Array_Of_Objects)
   is
      It : Object := SK.Cells.Stack_Top_Cell (Cells);
   begin
      for I in Items'Range loop
         Items (I) := SK.Cells.Car (Cells, It);
         if Items (I) = Boundary_Object then
            raise Constraint_Error with "crossed stack boundary";
         end if;
         It := SK.Cells.Cdr (Cells, It);
      end loop;
   end Top;

end SK.Stack;
