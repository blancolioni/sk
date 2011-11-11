with SK.Memory;

package body SK.Memory_Test is

   -----------------
   -- Test_Memory --
   -----------------

   procedure Test_Memory is
      use SK.Memory;
      M : Memory_Type := Create_Memory (1024, 0);
      X, Y : Object := Null_Object;
      Root : Object;
      Z    : Object;
   begin
      Allocate (M, Get_Bits (O_Pair), X, Y, Root);
      Set_Register (M, 0, Root);
      Set_Register (M, 1, Root);
      for I in 1 .. 200 loop
         X := To_Object (I);
         Y := To_Object (I + 1000);
         Allocate (M, Get_Bits (O_Pair), X, Y, Z);
         Set_Cdr (M, To_Cell_Address (Get_Register (M, 1)), Z);
         Set_Register (M, 1, Z);
         Allocate (M, Get_Bits (O_Pair), X, Y, Z);
         Allocate (M, Get_Bits (O_Pair), X, Y, Z);
         Allocate (M, Get_Bits (O_Pair), X, Y, Z);
         Allocate (M, Get_Bits (O_Pair), X, Y, Z);
      end loop;
   end Test_Memory;

end SK.Memory_Test;
