with Ada.Text_IO;
with SK.Machine;

procedure SK_Run is

   M : SK.Machine.SK_Machine;

   procedure Test (Expr : String);

   procedure Test (Expr : String) is
      E : SK.Object;
   begin
      E := SK.Machine.Parse_String (M, Expr);
      Ada.Text_IO.Put_Line (SK.Machine.Show (M, E));
      E := SK.Machine.Compile (M, E);
      Ada.Text_IO.Put_Line (SK.Machine.Show (M, E));
      E := SK.Machine.Evaluate (M, E);
      Ada.Text_IO.Put_Line (SK.Machine.Show (M, E));
   end Test;

begin

   SK.Start_SK;
   --  SK.Memory_Test.Test_Memory;

   M := SK.Machine.Create_Machine (8192);

   Test ("S (B #intPlus I) I 2");
   Test ("S (K #intPlus) (B I I) 1 2");
   Test ("S (B #intPlus I) (K 1) 2");
   Test ("(\x.#intPlus x x) 5");
   Test ("(\x.\y.#intPlus x y) 5 3");
   Test ("if true 0 1");
   Test ("eq? 0 0 1 2");
   Test ("ifz (eq? 2 (#intPlus 1 1)) 2 3");
   Test ("(Y (\f.\x.ifz x x (#intPlus x (f (#intMinus x 1))))) 10");
   Test ("(\x.#intPlus x x) (#intPlus 1 2)");
   Test ("(Y (\f.\x.x 0 (\h.\t.#intPlus 1 (f t)))) (cons 0 (cons 1 false))");

end SK_Run;
