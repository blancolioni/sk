with System;

package SK is

   Type_Error : exception;
   Evaluation_Error : exception;

   Max_Int : constant := 2 ** 29 - 1;

   type Combinator is (I, S, K, B, C, Sd, Bd, Cd);

   type Object_Class is
     (O_Integer,
      O_Immediate,
      O_Application,
      O_Lambda);

   type Function_Id is private;

   type Type_Id is private;

   type Object is private;

   function Is_Immediate (Item : Object) return Boolean;
   function Is_Null (Item : Object) return Boolean;
   function Is_Combinator (Item : Object) return Boolean;
   function Is_Number (Item : Object) return Boolean;
   function Is_Integer (Item : Object) return Boolean;
   function Is_Function (Item : Object) return Boolean;
   function Is_Application (Item : Object) return Boolean;
   function Is_Symbol (Item : Object) return Boolean;
   function Is_Lambda (Item : Object) return Boolean;
   function Is_Atomic (Item : Object) return Boolean;
   function Is_Character (Item : Object) return Boolean;

   function Get_Combinator (Item : Object) return Combinator;
   function Get_Name (Item : Object) return String;
   function Get_Symbol (Item : String) return Object;
   function Get_Character (Item : Object) return Character;

   function To_Boolean (Item : Object) return Boolean;
   function Get_Integer (Item : Object) return Integer;
   function Get_Function_Id (Item : Object) return Function_Id;

   function Equal (Left, Right : Object) return Boolean;

   function I return Object;
   function K return Object;
   function S return Object;
   function B return Object;
   function C return Object;
   function Sd return Object;
   function Bd return Object;
   function Cd return Object;

   function Next_Argument return Object;
   function Last_Argument return Object;

   function Select_Object (Num_Choices : Positive) return Object;
   function Is_Select (Item : Object) return Boolean;
   function Select_Choice_Count (Item : Object) return Positive;

   function Null_Object return Object;
   function Empty_List_Object return Object;
   function Undefined_Object return Object;
   function Fail_Object return Object;
   function Unbound_Object return Object;
   function Dont_Care_Object return Object;

   function To_Object (Num : Integer) return Object;
   function To_Object (B : Boolean) return Object;
   function To_Object (Id  : Function_Id) return Object;
   function To_Object (Ch : Character) return Object;
   function To_Object (Item : System.Address) return Object;

   function Initial_World return Object;
   function Next_World (Current_World : Object) return Object;

   function Max_Integer return Integer;
   function Min_Integer return Integer;

   function Hex_Image (Item : Object) return String;

   procedure Start_SK;

   type Array_Of_Objects is array (Positive range <>) of Object;

   function Unchecked_To_Object (Value : Integer) return Object;
   --  Converts a value to an object

private

   --  Format of a single 32-bit cell
   --  2 LSBs indicate type
   --     00: 30 bit immediate (e.g. an integer, pointer, 30-bit float)
   --     01: other immediate (bits 7-3 clarify)
   --     10: application
   --     11: lambda abstraction

   --  For non-Integer immediates, bits 7-3 on low octet indicate type
   --     00000: symbol, high 24 bits of cell are index
   --     00001: function, high 24 bits of cell are index
   --     00010: 8 bit character constant, octet 1 contains code
   --     00011: wide character constant, to be defined
   --     00100: next-argument
   --     00101: last-argument
   --     00110: pick-n; octet n contains choice count
   --     00111: world token
   --     01000 .. 01110: to be defined
   --     01111: extended constant in high 24 bits
   --     10000: nil
   --     10001: empty list
   --     10010: fail
   --     10011: unbound
   --     10100: don't care
   --     10101: undefined
   --     10110: garbage
   --     10111: boundary
   --     11000: I
   --     11001: S
   --     11010: K
   --     11011: B
   --     11100: C
   --     11101: S'
   --     11110: B'
   --     11111: C'

   Object_Type_Bits : constant := 2;
   Object_Type_Mask : constant := 2**Object_Type_Bits - 1;

   function Get_Bits (Class : Object_Class)
                      return Object;

   function Get_Object_Class (Item : Object) return Object_Class;

   function Boundary_Object return Object;
   --  Stack boundary object

   Object_Bits : constant := 32;
   type Object is mod 2**Object_Bits;
   for Object'Size use Object_Bits;

   function Initial_World return Object
   is (2#0001_1101#);

   function Next_World (Current_World : Object) return Object
   is (Current_World + 16#0010#);

   type Object_Pair is
      record
         Left, Right : Object;
      end record;

   for Object_Pair'Size use 64;

   type Function_Id is new Object;
   type Symbol_Id   is new Object;
   type Type_Id     is new Object;

   function Get_Symbol_Id (Item : Object) return Symbol_Id;

   Symbol_Bits     : constant := 2#00000001#;
   Function_Bits   : constant := 2#00001001#;
   Character_Bits  : constant := 2#00010001#;

   Combinator_Base : constant := 2#11000001#;
   Constant_Base   : constant := 2#10000001#;

   Pick_Bits : constant := 2#0011001#;

   type Constant_Object_Classification is
     (Nil_Object,
      Empty_List_Object,
      Fail_Object,
      Unbound_Object,
      Dont_Care_Object,
      Undefined_Object,
      Garbage_Object,
      Boundary_Object,
      I_Object,
      S_Object,
      K_Object,
      B_Object,
      C_Object,
      S_Dash_Object,
      B_Dash_Object,
      C_Dash_Object);

   function Make_Combinator (Comb : Combinator) return Object
   is (Combinator_Base + Combinator'Pos (Comb) * 8);

   function I return Object is (Make_Combinator (I));
   function K return Object is (Make_Combinator (K));
   function S return Object is (Make_Combinator (S));
   function B return Object is (Make_Combinator (B));
   function C return Object is (Make_Combinator (C));
   function Sd return Object is (Make_Combinator (Sd));
   function Bd return Object is (Make_Combinator (Bd));
   function Cd return Object is (Make_Combinator (Cd));

end SK;
