with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with System.Storage_Elements;

with SK.Symbols;

package body SK is

   type Octet_Index is range 0 .. 3;
   function Get_Octet (From  : Object;
                       Index : Octet_Index)
                      return Object;

   function Make_Constant (Const : Constant_Object_Classification)
                          return Object;
   pragma Inline (Make_Constant);

   function Make_Symbol (Id : Symbol_Id) return Object;

   ---------------------
   -- Boundary_Object --
   ---------------------

   function Boundary_Object return Object is
   begin
      return Make_Constant (Boundary_Object);
   end Boundary_Object;

   ----------------------
   -- Dont_Care_Object --
   ----------------------

   function Dont_Care_Object return Object is
   begin
      return Make_Constant (Dont_Care_Object);
   end Dont_Care_Object;

   -----------------------
   -- Empty_List_Object --
   -----------------------

   function Empty_List_Object return Object is
   begin
      return Make_Constant (Empty_List_Object);
   end Empty_List_Object;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : Object) return Boolean is
   begin
      return Left = Right;
   end Equal;

   -----------------
   -- Fail_Object --
   -----------------

   function Fail_Object return Object is
   begin
      return Make_Constant (Fail_Object);
   end Fail_Object;

   --------------
   -- Get_Bits --
   --------------

   function Get_Bits (Class : Object_Class)
                      return Object
   is
   begin
      return Object_Class'Pos (Class);
   end Get_Bits;

   -------------------
   -- Get_Character --
   -------------------

   function Get_Character (Item : Object) return Character is
   begin
      return Character'Val (Get_Octet (Item, 1));
   end Get_Character;

   --------------------
   -- Get_Combinator --
   --------------------

   function Get_Combinator (Item : Object) return Combinator is
   begin
      pragma Assert (Is_Combinator (Item));
      return Combinator'Val ((Item - Combinator_Base) / 8);
   end Get_Combinator;

   ---------------------
   -- Get_Function_Id --
   ---------------------

   function Get_Function_Id (Item : Object) return Function_Id is
   begin
      pragma Assert (Is_Function (Item));
      return Function_Id (Item / 256);
   end Get_Function_Id;

   -----------------
   -- Get_Integer --
   -----------------

   function Get_Integer (Item : Object) return Integer is
      function Convert is
         new Ada.Unchecked_Conversion (Object, Integer);
   begin
      if not Is_Integer (Item) then
         Ada.Text_IO.Put_Line ("Error: expected integer but got " &
                               Hex_Image (Item));
         raise Constraint_Error;
      end if;
      pragma Assert (Is_Integer (Item));
      if (Item and 2**31) = 0 then
         return Integer (Item / 2**Object_Type_Bits);
      else
         return Convert (16#E000# + (Item / 2**Object_Type_Bits));
      end if;
   end Get_Integer;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : Object) return String is
   begin
      if not Is_Symbol (Item) then
         raise Constraint_Error with
           "attempted to get a name from " & Hex_Image (Item);
      end if;
      pragma Assert (Is_Symbol (Item));
      if Get_Symbol_Id (Item) = 0 then
         return "<null symbol id>";
      else
         return Symbols.Get_Name (Get_Symbol_Id (Item));
      end if;
   end Get_Name;

   ----------------------
   -- Get_Object_Class --
   ----------------------

   function Get_Object_Class (Item : Object) return Object_Class is
   begin
      return Object_Class'Val (Item and Object_Type_Mask);
   end Get_Object_Class;

   ---------------
   -- Get_Octet --
   ---------------

   function Get_Octet (From  : Object;
                       Index : Octet_Index)
                      return Object
   is
   begin
      return (From / Object (2**Natural (8 * Index))) and 255;
   end Get_Octet;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Item : String) return Object is
   begin
      return Make_Symbol (SK.Symbols.Get_Symbol_Id (Item));
   end Get_Symbol;

   -------------------
   -- Get_Symbol_Id --
   -------------------

   function Get_Symbol_Id (Item : Object) return Symbol_Id is
   begin
      pragma Assert (Is_Symbol (Item));
      return Symbol_Id (Item / 256);
   end Get_Symbol_Id;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Item : Object) return String is
      Tmp : Object := Item;
      Result : String (1 .. 8);

      function Hex_Digit (Item : Object) return Character;
      --  Item should be in range 0 .. 15

      ---------------
      -- Hex_Digit --
      ---------------

      function Hex_Digit (Item : Object) return Character is
         Hex_Digits : constant String := "0123456789ABCDEF";
      begin
         return Hex_Digits (Positive (Item + 1));
      end Hex_Digit;

   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digit (Tmp mod 16);
         Tmp := Tmp / 16;
      end loop;
      return Result;
   end Hex_Image;

   --------------------
   -- Is_Application --
   --------------------

   function Is_Application (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) = O_Application;
   end Is_Application;

   ---------------
   -- Is_Atomic --
   ---------------

   function Is_Atomic (Item : Object) return Boolean is
   begin
      return Is_Immediate (Item);
   end Is_Atomic;

   ------------------
   -- Is_Character --
   ------------------

   function Is_Character (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) = O_Immediate
        and then Get_Octet (Item, 0) = Character_Bits;
   end Is_Character;

   -------------------
   -- Is_Combinator --
   -------------------

   function Is_Combinator (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) = O_Immediate
        and then Item / 8 in 2#11000# .. 2#11111#;
   end Is_Combinator;

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function (Item : Object) return Boolean is
   begin
      return Get_Octet (Item, 0) = Function_Bits;
   end Is_Function;

   ------------------
   -- Is_Immediate --
   ------------------

   function Is_Immediate (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) in
        O_Integer .. O_Immediate;
   end Is_Immediate;

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) = O_Integer;
   end Is_Integer;

   ---------------
   -- Is_Lambda --
   ---------------

   function Is_Lambda (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) = O_Lambda;
   end Is_Lambda;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Item : Object) return Boolean is
   begin
      return Item = Null_Object;
   end Is_Null;

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number (Item : Object) return Boolean is
   begin
      return Is_Integer (Item);
   end Is_Number;

   -------------
   -- Is_Pick --
   -------------

   function Is_Select (Item : Object) return Boolean is
   begin
      return Item mod 256 = Pick_Bits
        and then Item / 256 mod 256 > 1;
   end Is_Select;

   ---------------
   -- Is_Symbol --
   ---------------

   function Is_Symbol (Item : Object) return Boolean is
   begin
      return Get_Octet (Item, 0) = Symbol_Bits;
   end Is_Symbol;

   -------------------
   -- Last_Argument --
   -------------------

   function Last_Argument return Object is
   begin
      return 2#0010101#;
   end Last_Argument;

   -------------------
   -- Make_Constant --
   -------------------

   function Make_Constant (Const : Constant_Object_Classification)
                          return Object
   is
   begin
      return Constant_Base + Constant_Object_Classification'Pos (Const) * 8;
   end Make_Constant;

   -----------------
   -- Make_Symbol --
   -----------------

   function Make_Symbol (Id : Symbol_Id) return Object is
   begin
      return Object (Id) * 256 + Symbol_Bits;
   end Make_Symbol;

   -----------------
   -- Max_Integer --
   -----------------

   function Max_Integer return Integer is
   begin
      return 2**29 - 1;
   end Max_Integer;

   -----------------
   -- Min_Integer --
   -----------------

   function Min_Integer return Integer is
   begin
      return -2**29;
   end Min_Integer;

   -------------------
   -- Next_Argument --
   -------------------

   function Next_Argument return Object is
   begin
      return 2#0010001#;
   end Next_Argument;

   -----------------
   -- Null_Object --
   -----------------

   function Null_Object return Object is
   begin
      return Make_Constant (Nil_Object);
   end Null_Object;

   ---------------
   -- Num_Picks --
   ---------------

   function Select_Choice_Count (Item : Object) return Positive is
      pragma Assert (Is_Select (Item));
   begin
      return Positive (Item / 256);
   end Select_Choice_Count;

   ----------
   -- Pick --
   ----------

   function Select_Object (Num_Choices : Positive) return Object is
   begin
      return Object (Num_Choices) * 256 + Pick_Bits;
   end Select_Object;

   --------------
   -- Start_SK --
   --------------

   procedure Start_SK is
   begin
      null;
   end Start_SK;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Item : Object) return Boolean is
   begin
      return Item /= Null_Object and then Item /= 0;
   end To_Boolean;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (B : Boolean) return Object is
      --  Working around an Aquarius bug
      Integer_Value : constant Integer := Boolean'Pos (B);
   begin
      return To_Object (Integer_Value);
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Ch : Character) return Object is
   begin
      return Character'Pos (Ch) * 256 + Character_Bits;
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Item : System.Address) return Object is
   begin
      return To_Object (Integer (System.Storage_Elements.To_Integer (Item)));
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Num : Integer) return Object is
      function Convert is
         new Ada.Unchecked_Conversion (Integer, Object);
   begin
      return Convert (Num) * (2**Object_Type_Bits) and not Object_Type_Mask;
   end To_Object;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Id : Function_Id) return Object is
   begin
      return Object (Id) * 256 + Function_Bits;
   end To_Object;

   --------------------
   -- Unbound_Object --
   --------------------

   function Unbound_Object return Object is
   begin
      return Make_Constant (Unbound_Object);
   end Unbound_Object;

   -------------------------
   -- Unchecked_To_Object --
   -------------------------

   function Unchecked_To_Object (Value : Integer) return Object is
   begin
      return Object (Value);
   end Unchecked_To_Object;

   ----------------------
   -- Undefined_Object --
   ----------------------

   function Undefined_Object return Object is
   begin
      return Make_Constant (Undefined_Object);
   end Undefined_Object;

end SK;
