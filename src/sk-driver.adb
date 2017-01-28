with Ada.Text_IO;

with SK.Debug;
with SK.Machine;
with SK.Objects;
with SK.Parser;

with SK.Objects.Symbols;

procedure SK.Driver is
   Trials  : constant := 10_000;
   Machine : constant SK.Machine.SK_Machine :=
               SK.Machine.Create (32 * 1024);
   Test    : constant String :=
               "((\x.\xs.\k1.\k2.k2 x xs) 1 2) 0 (\y.\ys.y)";
begin

   Machine.Push
     (SK.Parser.Parse_String (Machine.all, "\f.(\x.f (x x)) (\x.f (x x))"));
   Ada.Text_IO.Put_Line (Machine.Show (Machine.Top));

   Machine.Compile;

   Ada.Text_IO.Put_Line (Machine.Show (Machine.Top));

   Machine.Define_Symbol (SK.Objects.Symbols.Get_Symbol_Id ("Y"),
                          Machine.Pop);

   if False then
      SK.Debug.Enable (SK.Debug.Eval);
      SK.Debug.Enable (SK.Debug.Compiler);
   end if;

   for I in 1 .. Trials loop
      Machine.Push
        (SK.Parser.Parse_String (Machine.all, Test));
      declare
         E : constant SK.Objects.Object := Machine.Top;
         S : constant String := Machine.Show (E);
      begin
         if S /= "(\x.\xs.\k1.\k2.k2 x xs) 1 2 0 (\y.\ys.y)" then
            Ada.Text_IO.Put_Line
              ("error in trial" & I'Img & ": " & S);
         end if;
      end;

      Machine.Compile;

      declare
         S : constant String := Machine.Show (Machine.Top);
      begin
         if S /= "B (B K) (B C (C I)) 1 2 0 K" then
            Ada.Text_IO.Put_Line
              ("error in trial" & I'Img & ": " & S);
         end if;
      end;

      declare
         X : SK.Objects.Object := Machine.Pop;
      begin
         X := Machine.Evaluate (X);
         Machine.Push (X);
      end;

      declare
         S : constant String := Machine.Show (Machine.Top);
      begin
         if S /= "1" then
            Ada.Text_IO.Put_Line
              ("error in trial" & I'Img & ": " & S);
         end if;
      end;

      Machine.Drop;

   end loop;

   if False then
      SK.Debug.Enable (SK.Debug.Eval);
   end if;

   Machine.Push (SK.Objects.To_Selection_Object (2));
   Machine.Push (1);
   Machine.Apply;
   Machine.Push (2);
   Machine.Apply;
   Machine.Push (3);
   Machine.Apply;

   Ada.Text_IO.Put_Line
     (Machine.Show (Machine.Evaluate (Machine.Pop)));

   Machine.Push
     (SK.Parser.Parse_String
        (Machine.all,
         "\x.#intPlus x x"));
   Machine.Compile;
   Ada.Text_IO.Put_Line (Machine.Show (Machine.Pop));

   Machine.Push
     (SK.Parser.Parse_String
        (Machine.all,
         "(\x.\y.#intPlus x y) 1 2"));
   Machine.Compile;
   Ada.Text_IO.Put_Line (Machine.Show (Machine.Pop));

   Machine.Push
     (SK.Parser.Parse_String
        (Machine.all,
         "(\x.#intPlus x x) ((\x.\y.#intPlus x y) 1 2)"));
   Machine.Compile;
   Machine.Link;

   Ada.Text_IO.Put_Line (Machine.Show (Machine.Top));
   Ada.Text_IO.Put_Line (Machine.Show (Machine.Evaluate (Machine.Pop)));

   Ada.Text_IO.Put_Line
     (Machine.Show
        (Machine.Get_Symbol
             (SK.Objects.Symbols.Get_Symbol_Id ("Y"))));

   Machine.Report_State;
end SK.Driver;
