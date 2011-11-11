package SK.Big_Integers is

   function Value (Image : String) return Object;

   function To_Big_Integer (Num : Integer) return Object;
   function To_Integer (Num : Object) return Integer;

   function Big_Add (Left, Right : Object) return Object;
   function Big_Subtract (Left, Right : Object) return Object;
   function Big_Multiply (Left, Right : Object) return Object;
   function Big_Divide (Left, Right : Object) return Object;
   function Big_Modulus (Left, Right : Object) return Object;

end SK.Big_Integers;
