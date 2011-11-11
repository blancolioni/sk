package body SK64.Memory is

   Top                     : Address;
   Memory_Low, Memory_High : Address;
   Space_Size              : Address;
   To_Space, From_Space    : Address;
   Free, Scan              : Address;
   Temp                    : Address;

   for Memory_Low'Address use To_Address (16#0030#);
   for Memory_High'Address use To_Address (16#0032#);
   for Space_Size'Address use To_Address (16#0034#);
   for To_Space'Address use To_Address (16#0036#);
   for From_Space'Address use To_Address (16#0038#);
   for Free'Address use To_Address (16#003A#);
   for Top'Address use To_Address (16#003C#);
   for Scan'Address use To_Address (16#003E#);
   for Temp'Address use To_Address (16#0040#);

   type Register is (Root, Stack, Alloc_Car, Alloc_Cdr);
   Reg : array (Register) of Object;

   procedure Flip;
   function Copy (Item : Object) return Object;
   function Move (Item : Object) return Object;

   function To_Space (Item : Object) return Boolean;
   function From_Space (Item : Object) return Boolean;

   procedure Set_Car (Pair    : Object;
                      New_Car : Object);

   procedure Set_Cdr (Pair    : Object;
                      New_Cdr : Object);

   --------------
   -- Allocate --
   --------------

   procedure Allocate (X, Y    : in out Object;
                       Result  :    out Object)
   is
      Result : Object;
   begin
      if Top = Free then
         Reg (Alloc_Car) := X;
         Reg (Alloc_Cdr) := Y;
         Collect;
         X := Reg (Alloc_Car);
         Y := Reg (Alloc_Cdr);
      end if;

      Create_Pair (Result, Free);
      Set_Car (Result, X);
      Set_Cdr (Result, Y);
      Free := Free + 4;
   end Allocate;

   ------------------
   -- Can_Allocate --
   ------------------

   function Can_Allocate (Count : Positive) return Boolean is
   begin
      return Free + Address (Count) * 4 < Top;
   end Can_Allocate;

   -------------
   -- Collect --
   -------------

   procedure Collect is
   begin
      Flip;

      for I in Reg'Range loop
         Reg (I) := Move (Reg (I));
      end loop;

      while Scan < Free loop
         Set_Car (Scan, Move (Car (Scan)));
         Set_Cdr (Scan, Move (Cdr (Scan)));
         Scan := Scan + 4;
      end loop;

   end Collect;

   ----------
   -- Copy --
   ----------

   function Copy (Item : Object) return Object is
      Result : Object;
   begin
      Create_Pair (Result, Free);
      Set_Car (Result, Car (Item));
      Set_Cdr (Result, Cdr (Item));
      Free := Free + 4;
      return Result;
   end Copy;

   ----------
   -- Flip --
   ----------

   procedure Flip is
   begin
      Temp       := To_Space;
      To_Space   := From_Space;
      From_Space := Temp;
      Top        := To_Space + Space_Size;
      Free       := To_Space;
      Scan       := To_Space;
   end Flip;

   ----------------
   -- From_Space --
   ----------------

   function From_Space (Item : Object) return Boolean is
   begin
      return Is_Pair (Item) and then
        Item - 2 in From_Space .. From_Space + Space_Size - 1;
   end From_Space;

   ----------
   -- Move --
   ----------

   function Move (Item : Object) return Object is
   begin
      if not From_Space (Item) then
         return Item;
      else
         if not To_Space (Car (Item)) then
            Set_Car (Item, Copy (Car (Item)));
         end if;
         return Car (Item);
      end if;
   end Move;

   -------------
   -- Set_Car --
   -------------

   procedure Set_Car (Pair    : Object;
                      New_Car : Object)
   is
   begin
      Set_Word16 (Address (Pair - 2), Word16 (New_Car));
   end Set_Car;

   -------------
   -- Set_Cdr --
   -------------

   procedure Set_Cdr (Pair    : Object;
                      New_Cdr : Object)
   is
   begin
      Set_Word16 (Address (Pair), Word_16 (New_Cdr));
   end Set_Cdr;

  --------------
   -- To_Space --
   --------------

   function To_Space (Item : Object) return Boolean is
   begin
      return Is_Pair (Item) and then
        Item - 2 in To_Space .. To_Space + Space_Size - 1;
   end To_Space;

end SK64.Memory;



