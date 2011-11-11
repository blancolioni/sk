package body SK is

   function Get_Object_Class (Item : Object) return Object_Class;
   pragma Inline (Get_Object_Class);

   function Make_Combinator (Comb : Combinator) return Object;
   function Make_Constant (Const : Constant_Object_Classification)
                          return Object;
   pragma Inline (Make_Constant);

   function Make_Symbol (Id : Symbol_Id) return Object;


   --  Inline local subprograms come first

   ----------------------
   -- Get_Object_Class --
   ----------------------

   function Get_Object_Class (Item : Object) return Object_Class is
   begin
      return Object_Class'Val (Item and Object_Type_Mask);
   end Get_Object_Class;

   -------------------
   -- Make_Constant --
   -------------------

   function Make_Constant (Const : Constant_Object_Classification)
                          return Object
   is
   begin
      return Constant_Base + Constant_Object_Classification'Pos (Const) * 8;
   end Make_Constant;

   --------------------
   -- Is_Application --
   --------------------

   function Is_Application (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) = O_Application;
   end Is_Application;

   ------------------
   -- Is_Immediate --
   ------------------

   function Is_Immediate (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) in
        O_Integer .. O_Immediate;
   end Is_Immediate;

   -------
   -- B --
   -------

   function B return Object is
   begin
      return Make_Combinator (B);
   end B;

   --------
   -- Bd --
   --------

   function Bd return Object is
   begin
      return Make_Combinator (Bd);
   end Bd;

  ---------------------
   -- Boundary_Object --
   ---------------------

   function Boundary_Object return Object is
   begin
      return Make_Constant (Boundary_Object);
   end Boundary_Object;

   -------
   -- C --
   -------

   function C return Object is
   begin
      return Make_Combinator (C);
   end C;

   --------
   -- Cd --
   --------

   function Cd return Object is
   begin
      return Make_Combinator (Cd);
   end Cd;

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

   --------------------
   -- Garbage_Object --
   --------------------

   function Garbage_Object return Object is
   begin
      return Make_Constant (Garbage_Object);
   end Garbage_Object;

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

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class (Item : Object) return Object_Class is
   begin
      return Get_Object_Class (Item);
   end Get_Class;

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

   -----------------
   -- Get_Type_Id --
   -----------------

   function Get_Type_Id (Item : Object) return Type_Id is
   begin
      return Type_Id (Item / 256);
   end Get_Type_Id;

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

   -------
   -- I --
   -------

   function I return Object is
   begin
      return Make_Combinator (I);
   end I;

   ---------------
   -- Is_Atomic --
   ---------------

   function Is_Atomic (Item : Object) return Boolean is
   begin
      return Is_Immediate (Item) or
        Get_Object_Class (Item) = O_Float;
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


   --------------
   -- Is_Float --
   --------------

   function Is_Float (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) = O_Float;
   end Is_Float;

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function (Item : Object) return Boolean is
   begin
      return Get_Octet (Item, 0) = Function_Bits;
   end Is_Function;

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
      return Is_Integer (Item) or Is_Float (Item);
   end Is_Number;

   -------------
   -- Is_Pair --
   -------------

   function Is_Pair (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) = O_Pair;
   end Is_Pair;

   ---------------
   -- Is_Symbol --
   ---------------

   function Is_Symbol (Item : Object) return Boolean is
   begin
      return Get_Octet (Item, 0) = Symbol_Bits;
   end Is_Symbol;

   ---------------------
   -- Is_User_Defined --
   ---------------------

   function Is_User_Defined (Item : Object) return Boolean is
   begin
      return Get_Object_Class (Item) = O_User_Defined;
   end Is_User_Defined;

   -------
   -- K --
   -------

   function K return Object is
   begin
      return Make_Combinator (K);
   end K;

   ---------------------
   -- Make_Combinator --
   ---------------------

   function Make_Combinator (Comb : Combinator) return Object is
   begin
      return Combinator_Base + Combinator'Pos (Comb) * 8;
   end Make_Combinator;

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
      return 2**28 - 1;
   end Max_Integer;

   -----------------
   -- Min_Integer --
   -----------------

   function Min_Integer return Integer is
   begin
      return -2**28;
   end Min_Integer;

   -----------------
   -- Null_Object --
   -----------------

   function Null_Object return Object is
   begin
      return Make_Constant (Nil_Object);
   end Null_Object;

   -------
   -- S --
   -------

   function S return Object is
   begin
      return Make_Combinator (S);
   end S;

   --------
   -- Sd --
   --------

   function Sd return Object is
   begin
      return Make_Combinator (Sd);
   end Sd;


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
      return Item /= Null_Object and Item /= 0;
   end To_Boolean;

   ---------------
   -- To_Object --
   ---------------

   function To_Object (B : Boolean) return Object is
   begin
      return To_Object (Integer'(Boolean'Pos (B)));
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

   ---------------
   -- To_Object --
   ---------------

   function To_Object (Id : Type_Id) return Object is
   begin
      return Object (Id * 256);
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

   -----------------------------
   -- User_Defined_Type_Class --
   -----------------------------

   function User_Defined_Type_Class return Object is
   begin
      return Object_Class'Pos (O_User_Defined);
   end User_Defined_Type_Class;

end SK;


