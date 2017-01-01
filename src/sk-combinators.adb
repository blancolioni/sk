with Ada.Text_IO;

with SK.Images;
with SK.Stack;

package body SK.Combinators is

   Debug_Combinators : constant Boolean := False;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Cells : SK.Cells.Managed_Cells;
                      Comb  : Combinator)
                      return Object
   is
      use SK.Stack;
      Args : Array_Of_Objects (1 .. 4);
      Ts   : Array_Of_Objects (1 .. 3);
      T, T0, T1, T2 : Object;
      X : Object;
   begin
      case Comb is
         when I =>
            if Minimum_Count (Cells, 1) then
               SK.Stack.Pop (Cells, Args (1));
               return SK.Cells.Cdr (Cells, Args (1));
            end if;
         when K =>
            if Minimum_Count (Cells, 2) then
               SK.Stack.Pop (Cells, Args (1 .. 2));
               SK.Cells.Set_Car (Cells, Args (2), I);
               SK.Cells.Set_Cdr (Cells, Args (2),
                                 SK.Cells.Cdr (Cells, Args (1)));
               return SK.Cells.Cdr (Cells, Args (1));
            end if;
         when S =>
            if Minimum_Count (Cells, 3) then
               SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 2));
               SK.Stack.Top (Cells, Args (1 .. 3));
               T := Args (3);
               X := SK.Cells.Cdr (Cells, T);
               SK.Cells.Set_Cdr (Cells, T, Ts (1));
               SK.Cells.Set_Cdr (Cells, Ts (1), X);
               SK.Cells.Set_Car (Cells, Ts (1),
                                 SK.Cells.Cdr (Cells, Args (2)));
               SK.Cells.Set_Car (Cells, T, Ts (2));
               SK.Cells.Set_Cdr (Cells, Ts (2), X);
               SK.Cells.Set_Car (Cells, Ts (2),
                                 SK.Cells.Cdr (Cells, Args (1)));
               SK.Stack.Drop (Cells, 3);
               if Debug_Combinators then
                  Ada.Text_IO.Put_Line ("S --> " & SK.Images.Image (Cells, T));
               end if;
               return T;
            end if;
         when B =>
            if Minimum_Count (Cells, 3) then
               SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 1));
               T0 := Ts (1);
               SK.Stack.Top (Cells, Args (1 .. 3));
               T := Args (3);
               X := SK.Cells.Cdr (Cells, T);
               SK.Cells.Set_Cdr (Cells, T, T0);
               SK.Cells.Set_Cdr (Cells, T0, X);
               SK.Cells.Set_Car (Cells, T0, SK.Cells.Cdr (Cells, Args (2)));
               SK.Cells.Set_Car (Cells, T, SK.Cells.Cdr (Cells, Args (1)));
               SK.Stack.Drop (Cells, 3);
               return T;
            end if;
         when C =>
            if Minimum_Count (Cells, 3) then
               SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 1));
               T0 := Ts (1);
               SK.Stack.Top (Cells, Args (1 .. 3));
               T := Args (3);
               X := SK.Cells.Cdr (Cells, T);
               SK.Cells.Set_Cdr (Cells, T, SK.Cells.Cdr (Cells, Args (2)));
               SK.Cells.Set_Car (Cells, T, T0);
               SK.Cells.Set_Cdr (Cells, T0, X);
               SK.Cells.Set_Car (Cells, T0, SK.Cells.Cdr (Cells, Args (1)));
               SK.Stack.Drop (Cells, 3);
               return T;
            end if;
         when Sd =>
            if Minimum_Count (Cells, 3) then
               SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 3));
               T0 := Ts (1);
               T1 := Ts (2);
               T2 := Ts (3);

               SK.Stack.Top (Cells, Args (1 .. 4));
               T := Args (4);
               X := SK.Cells.Cdr (Cells, T);
               SK.Cells.Set_Cdr (Cells, T, T0);
               SK.Cells.Set_Cdr (Cells, T0, X);
               SK.Cells.Set_Car (Cells, T0, SK.Cells.Cdr (Cells, Args (3)));
               SK.Cells.Set_Car (Cells, T, T1);
               SK.Cells.Set_Car (Cells, T1, SK.Cells.Cdr (Cells, Args (1)));
               SK.Cells.Set_Cdr (Cells, T1, T2);
               SK.Cells.Set_Cdr (Cells, T2, X);
               SK.Cells.Set_Car (Cells, T2, SK.Cells.Cdr (Cells, Args (2)));
               SK.Stack.Drop (Cells, 4);
               if Debug_Combinators then
                  Ada.Text_IO.Put_Line
                    ("S' --> " & SK.Images.Image (Cells, T));
               end if;
               return T;
            end if;
         when Bd =>
            if Minimum_Count (Cells, 3) then
               SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 2));
               T0 := Ts (1);
               T1 := Ts (2);
               SK.Stack.Top (Cells, Args (1 .. 4));
               T := Args (4);
               X := SK.Cells.Cdr (Cells, T);
               SK.Cells.Set_Cdr (Cells, T, T0);
               SK.Cells.Set_Car (Cells, T, SK.Cells.Cdr (Cells, Args (1)));
               SK.Cells.Set_Car (Cells, T0, SK.Cells.Cdr (Cells, Args (2)));
               SK.Cells.Set_Cdr (Cells, T0, T1);
               SK.Cells.Set_Car (Cells, T1, SK.Cells.Cdr (Cells, Args (3)));
               SK.Cells.Set_Cdr (Cells, T1, X);
               SK.Stack.Drop (Cells, 4);
               if Debug_Combinators then
                  Ada.Text_IO.Put_Line
                    ("B* --> " & SK.Images.Image (Cells, T));
               end if;
               return T;
            end if;
         when Cd =>
            if Minimum_Count (Cells, 3) then
               SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 2));
               T0 := Ts (1);
               T1 := Ts (2);
               SK.Stack.Top (Cells, Args (1 .. 4));
               T := Args (4);
               if Debug_Combinators then
                  Ada.Text_IO.Put_Line (SK.Images.Image (Cells, T));
               end if;
               X := SK.Cells.Cdr (Cells, T);
               SK.Cells.Set_Cdr (Cells, T, SK.Cells.Cdr (Cells, Args (3)));
               SK.Cells.Set_Car (Cells, T, T0);
               SK.Cells.Set_Car (Cells, T0, SK.Cells.Cdr (Cells, Args (1)));
               SK.Cells.Set_Cdr (Cells, T0, T1);
               SK.Cells.Set_Cdr (Cells, T1, X);
               SK.Cells.Set_Car (Cells, T1, SK.Cells.Cdr (Cells, Args (2)));
               SK.Stack.Drop (Cells, 4);
               if Debug_Combinators then
                  Ada.Text_IO.Put_Line
                    ("C' --> " & SK.Images.Image (Cells, T));
               end if;
               return T;
            end if;
      end case;

      return Fail_Object;

   end Evaluate;

end SK.Combinators;
