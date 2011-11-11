with System;                            use System;

package body SK64 is

   ---------
   -- Car --
   ---------

   function Car (Item : Object) return Object is
      Result : M6502.Word16;
   begin
      M6502.Get_Word16 (M6502.Address (Item), Result);
      return Object (Result);
   end Car;

   ---------
   -- Cdr --
   ---------

   function Cdr (Item : Object) return Object is
      Result : M6502.Word16;
   begin
      M6502.Get_Word16 (M6502.Address (Item and not 2), Result);
      return Object (Result);
   end Cdr;

   --------------------
   -- Get_Combinator --
   --------------------

   function Get_Combinator (Item : Object) return Combinator is
   begin
      return Combinator'Val ((Item / 16) and 7);
   end Get_Combinator;

   ---------------------
   -- Get_Function_Id --
   ---------------------

   function Get_Function_Id (Item : Object) return Function_Id is
   begin
      return Function_Id (Item / 256);
   end Get_Function_Id;

   ---------------------
   -- Get_Object_Type --
   ---------------------

   function Get_Object_Type (Item : Object) return Object_Type is
   begin
      return Object_Type'Val (Item mod 4);
   end Get_Object_Type;

   -------------------
   -- Is_Combinator --
   -------------------

   function Is_Combinator (Item : Object) return Boolean is
   begin
      return (Item and 16#0F#) = 16#05#;
   end Is_Combinator;

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function (Item : Object) return Boolean is
   begin
      return (Item and 16#0F#) = 01;
   end Is_Function;

   ----------------
   -- Is_Integer --
   ----------------

   function Is_Integer (Item : Object) return Boolean is
   begin
      return Get_Object_Type (Item) = Integer_Object;
   end Is_Integer;

   -------------
   -- Is_Pair --
   -------------

   function Is_Pair (Item : Object) return Boolean is
   begin
      return Get_Object_Type (Item) = Pair_Object;
   end Is_Pair;

   ------------------
   -- Is_Primitive --
   ------------------

   function Is_Primitive (Item : Object) return Boolean is
   begin
      return Get_Object_Type (Item) = Primitive_Object;
   end Is_Primitive;

end SK64;



