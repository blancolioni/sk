with Ada.Text_IO;

with SK.Images;
with SK.Interfaces;

package body SK.Test_Actions is
   
   function Execute_Putstr  (Args : Array_Of_Objects) return Object;
   
   --------------------
   -- Execute_Putstr --
   --------------------
   
   function Execute_Putstr  (Args : Array_Of_Objects) return Object is
   begin
      Ada.Text_IO.Put (SK.Images.Show_String (Args (Args'First)));
      return Null_Object;
   end Execute_Putstr;
   
   ------------------
   -- Test_Actions --
   ------------------
   
   procedure Test_Actions is
      Hello : constant Object :=
	Apply (Lambda ("x", Apply (Get_Symbol ("putstr"), Get_Symbol ("x"))),
	       String_To_List ("Hello" & Character'Val (10)));
      Define_Hello : constant Object :=
	Apply (Apply (Get_Symbol ("define"), Get_Symbol ("hello")),
	       Action (Hello));
      Call_Hello : constant Object := Get_Symbol ("hello");
      Result     : Object;
   begin
      SK.Interfaces.New_Function ("putstr", 1,
                                  Execute_Putstr'Access,
                                  True);

      Result := Eval (Define_Hello);
      Result := Eval (Call_Hello);
      Result := Eval (Call_Hello);
   end Test_Actions;

end SK.Test_Actions;
