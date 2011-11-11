package body SK.Big_Integers is

   -------------
   -- Big_Add --
   -------------

   function Big_Add (Left, Right : Object) return Object is
   begin
      return Left + Right;
   end Big_Add;

   ----------------
   -- Big_Divide --
   ----------------

   function Big_Divide (Left, Right : Object) return Object is
   begin
      return To_Big_Integer (Get_Integer (Left) / Get_Integer (Right));
   end Big_Divide;

   -----------------
   -- Big_Modulus --
   -----------------

   function Big_Modulus (Left, Right : Object) return Object is
   begin
      return To_Big_Integer (Get_Integer (Left) mod Get_Integer (Right));
   end Big_Modulus;

   ------------------
   -- Big_Multiply --
   ------------------

   function Big_Multiply (Left, Right : Object) return Object is
   begin
      return To_Big_Integer (Get_Integer (Left) * Get_Integer (Right));
   end Big_Multiply;

   ------------------
   -- Big_Subtract --
   ------------------

   function Big_Subtract (Left, Right : Object) return Object is
   begin
      return Left - Right;
   end Big_Subtract;

   --------------------
   -- To_Big_Integer --
   --------------------

   function To_Big_Integer (Num : Integer) return Object is
   begin
      return To_Object (Num);
   end To_Big_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Num : Object) return Integer is
   begin
      return 0;
   end To_Integer;

   -----------
   -- Value --
   -----------

   function Value (Image : String) return Object is
      Result :          Object := To_Big_Integer (0);
      Ten    : constant Object := To_Big_Integer (10);
   begin
      for I in Image'Range loop
         Result := Big_Add (Big_Multiply (Result, Ten),
                            To_Big_Integer (Character'Pos (Image (I)) -
                                            Character'Pos ('0')));
      end loop;
      return Result;
   end Value;

end SK.Big_Integers;
