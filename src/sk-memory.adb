with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with SK.Environments;

package body SK.Memory is

   procedure Flip (M : in out Memory_Type);
   --  Switch semispaces

   function To_Object (Address   : Cell_Address;
                       Type_Bits : Object)
                       return Object;
   --  Make an object from an address

   On_GC : GC_Callback;
   --  call this after garbage collection

   procedure Free_Cells is
      new Ada.Unchecked_Deallocation (Cell_Array,
                                      Cell_Array_Access);

   --------------
   -- Allocate --
   --------------

   procedure Allocate (M             : in out Memory_Type;
                       Type_Bits     : in     Object;
                       X, Y          : in out Object;
                       Result        :    out Object)
   is
      function Move (Item : Object) return Object;
      function Copy (Item : Cell_Address) return Cell_Address;
      function Car (Addr : Cell_Address) return Object;
      function Cdr (Addr : Cell_Address) return Object;

      function To_Space (Item : Object) return Boolean;
      function From_Space (Item : Object) return Boolean;

      pragma Inline (Car);
      pragma Inline (Cdr);

      ---------
      -- Car --
      ---------

      function Car (Addr : Cell_Address) return Object is
      begin
         return Car (M, Addr);
      end Car;

      ---------
      -- Cdr --
      ---------

      function Cdr (Addr : Cell_Address) return Object is
      begin
         return Cdr (M, Addr);
      end Cdr;

      ----------
      -- Copy --
      ----------

      function Copy (Item : Cell_Address) return Cell_Address is
         Result : constant Cell_Address := M.Free;
      begin
         Set_Car (M, M.Free, Car (Item));
         Set_Cdr (M, M.Free, Cdr (Item));
         M.Free := M.Free + 1;
         return Result;
      end Copy;

      ----------------
      -- From_Space --
      ----------------

      function From_Space (Item : Object) return Boolean is
      begin
         return not Is_Immediate (Item) and then
         To_Cell_Address (Item) in
           M.From_Space .. M.From_Space + M.Space_Size - 1;
      end From_Space;

      ----------
      -- Move --
      ----------

      function Move (Item : Object) return Object is
         Addr : Cell_Address;
      begin
         if not From_Space (Item) then
            return Item;
         else
            Addr := To_Cell_Address (Item);
            if not To_Space (Car (Addr)) then
               Set_Car (M, Addr, To_Object (Copy (Addr), Item));
            end if;
            return Car (Addr);
         end if;
      end Move;

      --------------
      -- To_Space --
      --------------

      function To_Space (Item : Object) return Boolean is
      begin
         return not Is_Immediate (Item) and then
         To_Cell_Address (Item) in
           M.To_Space .. M.To_Space + M.Space_Size - 1;
      end To_Space;


   begin
      if M.Free = M.Top then
         if M.Managed then
            Ada.Text_IO.Put_Line ("Garbage collecting ...");
            if On_GC /= null then
               On_GC.all;
            end if;
            Flip (M);

            for I in M.R'Range loop
               M.R (I) := Move (M.R (I));
            end loop;

            SK.Environments.Update_All (Move'Access);

            X := Move (X);
            Y := Move (Y);
            while M.Scan < M.Free loop
               Set_Car (M, M.Scan, Move (Car (M.Scan)));
               Set_Cdr (M, M.Scan, Move (Cdr (M.Scan)));
               M.Scan := M.Scan + 1;
            end loop;
            Ada.Text_IO.Put_Line ("Allocated cells: " &
                                    Cell_Address'Image (M.Scan - M.To_Space));
            if On_GC /= null then
               On_GC.all;
            end if;
         elsif M.Extensible then
            declare
               New_Top : constant Cell_Address :=
                 2 * (M.Top - M.To_Space) + M.To_Space;
               New_Cells : constant Cell_Array_Access :=
                 new Cell_Array (M.To_Space .. New_Top - 1);
            begin
               New_Cells (M.To_Space .. M.Top - 1) := M.Cells.all;
               Free_Cells (M.Cells);
               M.Cells := New_Cells;
               M.Top := New_Top;
            end;
         else
            raise Constraint_Error with "out of memory";
         end if;
      end if;
      Set_Car (M, M.Free, X);
      Set_Cdr (M, M.Free, Y);
      Result := To_Object (M.Free, Type_Bits);
      M.Free := M.Free + 1;
   end Allocate;

   ---------
   -- Car --
   ---------

   function Car
     (M    : Memory_Type;
      Addr : Cell_Address)
      return Object
   is
   begin
      return M.Cells (Addr).Left;
   exception
      when Constraint_Error =>
         raise Constraint_Error with
           "bad memory access in car: " & Hex_Image (Object (Addr)) &
           " not in range " & Hex_Image (Object (M.Cells'First)) &
           " .. " & Hex_Image (Object (M.Cells'Last));
   end Car;

   ---------
   -- Cdr --
   ---------

   function Cdr
     (M    : Memory_Type;
      Addr : Cell_Address)
      return Object
   is
   begin
      return M.Cells (Addr).Right;
   end Cdr;

   ---------------
   -- Copy_Cell --
   ---------------

   procedure Copy_Cell (M    : in out Memory_Type;
                        Cell : in out Object;
                        Dest : in     Cell_Address)
   is
      Addr : constant Cell_Address := To_Cell_Address (Cell);
   begin
      Set_Car (M, Dest, Car (M, Addr));
      Set_Cdr (M, Dest, Cdr (M, Addr));
      Cell := To_Object (Dest, Cell mod 4);
   end Copy_Cell;

   --------------------
   -- Copy_Structure --
   --------------------

   procedure Copy_Structure (From    : in     Memory_Type;
                             To      : in out Memory_Type;
                             Start   : in out Object)
   is

      procedure Copy (Item : in out Object);

      ----------
      -- Copy --
      ----------

      procedure Copy (Item : in out Object) is
         X, Y   : Object;
         Result : Object;
      begin
         if Is_Application (Item) or Is_Lambda (Item) then
            if To_Cell_Address (Item) in From.Cells'Range then
               X := Car (From, To_Cell_Address (Item));
               Y := Cdr (From, To_Cell_Address (Item));
               Copy (X);
               Copy (Y);
               Allocate (To, Item, X, Y, Result);
               Item := Result;
            end if;
         end if;
      end Copy;

   begin
      Copy (Start);
   end Copy_Structure;

   ------------------------------
   -- Create_Extensible_Memory --
   ------------------------------

   function Create_Extensible_Memory (Base_Address : Cell_Address)
                                     return Memory_Type
   is
      Top_Address : constant Cell_Address :=
        Base_Address + 65535;
      Cells       : constant Cell_Array_Access :=
        new Cell_Array (Base_Address .. Top_Address);
   begin
      return (Cells      => Cells,
              Space_Size => 0,
              To_Space   => Base_Address,
              From_Space => 0,
              Free       => Base_Address,
              Top        => Top_Address,
              Scan       => 0,
              R          => null,
              Managed    => False,
              Extensible => True);
   end Create_Extensible_Memory;

   ---------------------------
   -- Create_Managed_Memory --
   ---------------------------

   function Create_Managed_Memory (Cell_Count     : Cell_Address;
                                   Base_Address   : Cell_Address)
     return Memory_Type
   is
      Top_Address : constant Cell_Address :=
        Base_Address + Cell_Count - 1;
      Space_Size  : constant Cell_Address := Cell_Count / 2;
      To_Space    : constant Cell_Address := Base_Address;
      From_Space  : constant Cell_Address := Base_Address + Space_Size;
      Cells       : constant Cell_Array_Access :=
        new Cell_Array (Base_Address .. Top_Address);
   begin
      return (Cells        => Cells,
              Space_Size   => Space_Size,
              To_Space     => To_Space,
              From_Space   => From_Space,
              Free         => To_Space,
              Top          => To_Space + Space_Size,
              Scan         => 0,
              R            => new Register_Array'(others => 0),
              Managed      => True,
              Extensible   => False);
   end Create_Managed_Memory;

   ----------
   -- Flip --
   ----------

   procedure Flip (M : in out Memory_Type) is
      T : constant Cell_Address := M.To_Space;
   begin
      M.To_Space := M.From_Space;
      M.From_Space := T;
      M.Top  := M.To_Space + M.Space_Size;
      M.Free := M.To_Space;
      M.Scan := M.To_Space;
      Ada.Text_IO.Put_Line ("Flip: To_Space = " &
                            Hex_Image (Object (M.To_Space)));
      Ada.Text_IO.Put_Line ("Flip: From_Space = " &
                            Hex_Image (Object (M.From_Space)));
      Ada.Text_IO.Put_Line ("Flip: free = " &
                            Hex_Image (Object (M.Free)));
   end Flip;

   --------------------
   -- Get_Cell_Count --
   --------------------

   function Get_Cell_Count (From : Memory_Type) return Cell_Address is
   begin
      return Cell_Address (From.Cells.all'Length);
   end Get_Cell_Count;

   ------------------
   -- Get_Register --
   ------------------

   function Get_Register (M : Memory_Type;
                          R : Register)
                          return Object
   is
   begin
      return M.R (R);
   end Get_Register;

   --------------
   -- In_Range --
   --------------

   function In_Range (Mem : Memory_Type;
                      Item : Object)
                     return Boolean
   is
      Addr : constant Cell_Address := To_Cell_Address (Item);
   begin
      return Addr in Mem.Cells.all'Range;
   end In_Range;

   -------------
   -- Set_Car --
   -------------

   procedure Set_Car
     (M    : Memory_Type;
      Addr : Cell_Address;
      To   : Object)
   is
   begin
      M.Cells (Addr).Left := To;
   end Set_Car;

   -------------
   -- Set_Cdr --
   -------------

   procedure Set_Cdr
     (M    : Memory_Type;
      Addr : Cell_Address;
      To   : Object)
   is
   begin
      M.Cells (Addr).Right := To;
   end Set_Cdr;

   ---------------------
   -- Set_GC_Callback --
   ---------------------

   procedure Set_GC_Callback (Callback : GC_Callback) is
   begin
      On_GC := Callback;
   end Set_GC_Callback;

   ------------------
   -- Set_Register --
   ------------------

   procedure Set_Register (M     : in Memory_Type;
                           R     : in Register;
                           Value : in Object)
   is
   begin
      M.R (R) := Value;
   end Set_Register;

   ---------------------
   -- To_Cell_Address --
   ---------------------

   function To_Cell_Address (Item : Object) return Cell_Address is
      pragma Assert (not Is_Atomic (Item));
   begin
      return Cell_Address (Item / (2 ** Object_Type_Bits));
   end To_Cell_Address;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Address   : Cell_Address;
                       Type_Bits : Object)
                       return Object
   is
   begin
      return Object (Address) * (2 ** Object_Type_Bits) +
        (Type_Bits mod (2**Object_Type_Bits));
   end To_Object;

end SK.Memory;
