package SK is

   Type_Error : exception;
   Evaluation_Error : exception;

   Max_Int : constant := 2 ** 14 - 1;

   type Combinator is (I, S, K, B, C, Sd, Bd, Cd);

   type Function_Id is private;

   type Type_Id is private;

   type Object is private;

   function Is_Immediate (Item : Object) return Boolean;
   function Is_Combinator (Item : Object) return Boolean;
   function Is_Number (Item : Object) return Boolean;
   function Is_Integer (Item : Object) return Boolean;
   function Is_Float (Item : Object) return Boolean;
   function Is_Function (Item : Object) return Boolean;
   function Is_Application (Item : Object) return Boolean;
   function Is_Symbol (Item : Object) return Boolean;
   function Is_Atomic (Item : Object) return Boolean;

   function Get_Combinator (Item : Object) return Combinator;
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

   function To_Object (Num : Integer) return Object;
   function To_Object (Id  : Function_Id) return Object;

   function Max_Integer return Integer;
   function Min_Integer return Integer;

   procedure Start_SK;

   type Array_Of_Objects is array (Positive range <>) of Object;

   function Unchecked_To_Object (Value : Integer) return Object;
   --  Converts a value to an object

private

   --  Format of a single 16-bit cell
   --  2 LSBs indicate type
   --     00: 14 bit signed integer
   --     01: other immediate (see below)
   --     10: pair/application
   --     11: undefined

   --  Non-integer immediates: bits 3-2
   --     00: primitive function (defined by bits 15-8)
   --     01: combinator (defined by bits 6-4)
   --     10: 'pick' primitive; bits 15-8 are size
   --     11: undefined

   --  combinators
   --    000 I
   --    001 S
   --    010 K
   --    011 B
   --    100 C
   --    101 S'
   --    110 B*
   --    111 C'

   --  primitive function ids
   --    00: stack-boundary
   --    01: eq?
   --    02: neq?
   --    03: ifz
   --    04: ifnz
   --    05: intPlus
   --    06: intMinus
   --    07: intMul
   --    08: intDiv
   --    09: intMod
   --    0A: memPut
   --    0B: memGet

   Object_Type_Mask : constant := 3;

   type Object_Class is
     (O_Integer,
      O_Immediate,
      O_Pair,
      O_Undefined);

   function Get_Bits (Class : Object_Class)
                      return Object;

   function Get_Class (Item : Object) return Object_Class;

   function Boundary_Object return Object;
   --  Stack boundary object

   Object_Bits : constant := 16;
   type Object is mod 65536;
   for Object'Size use 16;

   type Object_Pair is
      record
         Left, Right : Object;
      end record;

   for Object_Pair'Size use 32;

   type Function_Id is new Object;

   pragma Inline (Is_Immediate);
   pragma Inline (Is_Application);

end SK;
