with Ada.Text_IO;

with SK.Images;
with SK.Stack;

package body SK.Compiler is

   Debug_Compiler       : constant Boolean := False;
   Debug_Optimisation   : constant Boolean := False;

   Use_Dash_Combinators : constant Boolean := False;

   -------------
   -- Compile --
   -------------

   function Compile
     (Cells : SK.Cells.Managed_Cells;
      Item  : Object)
      return Object
   is
      procedure Compile (E : Object);

      procedure Abstract_Expr (X : Object;
                               E : Object);

      function Optimise (E       : in     Object)
                        return Boolean;

      procedure Check_S (E     : in      Object;
                         X, Y  :     out Object;
                         Found :     out Boolean);

      procedure Check_K (E     : in      Object;
                         X     :     out Object;
                         Found :     out Boolean);

      procedure Check_B (E     : in      Object;
                         X, Y  :     out Object;
                         Found :     out Boolean);

      -------------------
      -- Abstract_Expr --
      -------------------

      procedure Abstract_Expr (X : Object;
                               E : Object)
      is
         T, T1, T2 : Object;
         Ts        : Array_Of_Objects (1 .. 2);
      begin
         if X /= Dont_Care_Object and then
           Get_Name (X) = "_"
         then
            Abstract_Expr (Dont_Care_Object, E);
         elsif E = X then
            SK.Stack.Push (Cells, I);
         else
            case Get_Object_Class (E) is
               when O_Integer | O_Immediate =>
                  SK.Cells.Allocate (Cells, O_Application, T);
                  SK.Cells.Set_Car (Cells, T, K);
                  SK.Cells.Set_Cdr (Cells, T, E);
                  SK.Stack.Push (Cells, T);
               when O_Application =>
                  SK.Stack.Push (Cells, E);
                  Abstract_Expr (X, SK.Cells.Car (Cells,
                    SK.Stack.Top (Cells)));
                  Abstract_Expr (X, SK.Cells.Cdr (Cells,
                    SK.Stack.Get (Cells, 2)));
                  SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 2));
                  T2 := SK.Stack.Pop (Cells);
                  T1 := SK.Stack.Pop (Cells);
                  SK.Stack.Drop (Cells, 1);
                  if Is_Application (T1) and then
                    (SK.Cells.Car (Cells, T1) = K)
                  then
                     if T2 = I then
                        T := SK.Cells.Cdr (Cells, T1);
                     elsif Is_Application (T2) and then
                       (SK.Cells.Car (Cells, T2) = K)
                     then
                        SK.Cells.Set_Cdr (Cells, Ts (1),
                                          SK.Cells.Cdr (Cells, T2));
                        SK.Cells.Set_Car (Cells, Ts (1),
                                          SK.Cells.Cdr (Cells, T1));
                        SK.Cells.Set_Cdr (Cells, Ts (2), Ts (1));
                        SK.Cells.Set_Car (Cells, Ts (2), K);
                        T := Ts (2);
                     else
                        SK.Cells.Set_Cdr (Cells, Ts (1), T2);
                        SK.Cells.Set_Car (Cells, Ts (1), Ts (2));
                        SK.Cells.Set_Cdr (Cells, Ts (2),
                                          SK.Cells.Cdr (Cells, T1));
                        SK.Cells.Set_Car (Cells, Ts (2), B);
                        T := Ts (1);
                     end if;
                  elsif Is_Application (T2) and then
                    SK.Cells.Car (Cells, T2) = K
                  then
                     SK.Cells.Set_Cdr (Cells, Ts (1),
                                       SK.Cells.Cdr (Cells, T2));
                     SK.Cells.Set_Car (Cells, Ts (1), Ts (2));
                     SK.Cells.Set_Cdr (Cells, Ts (2), T1);
                     SK.Cells.Set_Car (Cells, Ts (2), C);
                     T := Ts (1);
                  else
                     SK.Cells.Set_Cdr (Cells, Ts (1), T2);
                     SK.Cells.Set_Car (Cells, Ts (1), Ts (2));
                     SK.Cells.Set_Cdr (Cells, Ts (2), T1);
                     SK.Cells.Set_Car (Cells, Ts (2), S);
                     T := Ts (1);
                  end if;

                  if Debug_Optimisation then
                     Ada.Text_IO.Put_Line (SK.Images.Image (Cells, T));
                  end if;

                  SK.Stack.Push (Cells, T);

               when O_Lambda =>
                  Abstract_Expr (SK.Cells.Car (Cells, E),
                                 SK.Cells.Cdr (Cells, E));
                  SK.Stack.Pop (Cells, T);
                  Abstract_Expr (X, T);
            end case;
         end if;
      end Abstract_Expr;

      -------------
      -- Check_B --
      -------------

      procedure Check_B (E     : in      Object;
                         X, Y  :     out Object;
                         Found :     out Boolean)
      is
      begin
         if Is_Application (E) and then
           Is_Application (SK.Cells.Car (Cells, E)) and then
           SK.Cells.Car (Cells, SK.Cells.Car (Cells, E)) = B
         then
            Found := True;
            Y := SK.Cells.Cdr (Cells, E);
            X := SK.Cells.Cdr (Cells, SK.Cells.Car (Cells, E));
         else
            Found := False;
         end if;
      end Check_B;

      -------------
      -- Check_K --
      -------------

      procedure Check_K (E     : in      Object;
                         X     :     out Object;
                         Found :     out Boolean)
      is
      begin
         if Is_Application (E) and then
           SK.Cells.Car (Cells, E) = K
         then
            Found := True;
            X := SK.Cells.Cdr (Cells, E);
         else
            Found := False;
         end if;
      end Check_K;

      -------------
      -- Check_S --
      -------------

      procedure Check_S (E     : in      Object;
                         X, Y  :     out Object;
                         Found :     out Boolean)
      is
      begin
         if Is_Application (E) and then
           Is_Application (SK.Cells.Car (Cells, E)) and then
           SK.Cells.Car (Cells, SK.Cells.Car (Cells, E)) = S
         then
            Found := True;
            Y := SK.Cells.Cdr (Cells, E);
            X := SK.Cells.Cdr (Cells, SK.Cells.Car (Cells, E));
         else
            Found := False;
         end if;
      end Check_S;

      -------------
      -- Compile --
      -------------

      procedure Compile (E : Object) is
         T : Object;
      begin
         case Get_Object_Class (E) is
            when O_Integer | O_Immediate =>
               SK.Stack.Push (Cells, E);
            when O_Application =>
               SK.Stack.Push (Cells, E);
               Compile (SK.Cells.Car (Cells, SK.Stack.Top (Cells)));
               Compile (SK.Cells.Cdr (Cells, SK.Stack.Get (Cells, 2)));
               SK.Cells.Allocate (Cells, O_Application, T);
               SK.Cells.Set_Cdr (Cells, T, SK.Stack.Pop (Cells));
               SK.Cells.Set_Car (Cells, T, SK.Stack.Pop (Cells));
               SK.Stack.Drop (Cells, 1);
               SK.Stack.Push (Cells, T);
            when O_Lambda =>
               Abstract_Expr (SK.Cells.Car (Cells, E),
                              SK.Cells.Cdr (Cells, E));
         end case;
      end Compile;

      --------------
      -- Optimise --
      --------------

      function Optimise (E       : in     Object)
                        return Boolean
      is
         X, Y, Z : Object;
         P, Q, R : Object;
         Ts      : Array_Of_Objects (1 .. 3);
         Match   : Boolean;
         K_X     : Boolean;
         K_Y     : Boolean;
         B_P_Q   : Boolean;
         Left_Changed, Right_Changed : Boolean := False;

      begin
--                                 (Cells, SK.Stack.Top (Cells)));
         if Is_Application (E) then
            Check_S (E, X, Y, Match);
            if Match then
               if Debug_Optimisation then
                  Ada.Text_IO.Put_Line ("Optimise: " &
                                          SK.Images.Image (Cells, E));
               end if;

               Check_K (X, P, K_X);
               Check_K (Y, Q, K_Y);
               if K_X and K_Y then
                  SK.Stack.Push (Cells, E);
                  SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 2));
                  SK.Cells.Set_Car (Cells, Ts (1), P);
                  SK.Cells.Set_Cdr (Cells, Ts (1), Q);
                  SK.Cells.Set_Car (Cells, Ts (2), K);
                  SK.Cells.Set_Cdr (Cells, Ts (2), Ts (1));
                  SK.Stack.Drop (Cells, 1);
                  SK.Stack.Push (Cells, Ts (2));
                  if Debug_Optimisation then
                     Ada.Text_IO.Put_Line ("S (K p) (K q): " &
                                             SK.Images.Image (Cells, Ts (2)));
                  end if;

                  return True;
               elsif K_X then
                  if Y = I then
                     if Debug_Optimisation then
                        Ada.Text_IO.Put_Line ("S (K p) I: " &
                                                SK.Images.Image (Cells, P));
                     end if;
                     SK.Stack.Push (Cells, P);
                     return True;
                  else
                     Check_B (Y, Q, R, B_P_Q);
                     if Use_Dash_Combinators and B_P_Q then
                        SK.Stack.Push (Cells, E);
                        SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 3));
                        SK.Cells.Set_Car (Cells, Ts (1), Bd);
                        SK.Cells.Set_Cdr (Cells, Ts (1), P);
                        SK.Cells.Set_Car (Cells, Ts (2), Ts (1));
                        SK.Cells.Set_Cdr (Cells, Ts (2), Q);
                        SK.Cells.Set_Car (Cells, Ts (3), Ts (2));
                        SK.Cells.Set_Cdr (Cells, Ts (3), R);
                        SK.Stack.Drop (Cells, 1);
                        SK.Stack.Push (Cells, Ts (3));
                        if Debug_Optimisation then
                           Ada.Text_IO.Put_Line ("S (K p) (B q r): " &
                                                   SK.Images.Image
                                                   (Cells, Ts (3)));
                        end if;
                        return True;
                     else
                        SK.Cells.Set_Cdr (Cells, SK.Cells.Car (Cells, E), P);
                        SK.Cells.Set_Car (Cells, SK.Cells.Car (Cells, E), B);
                        if Debug_Optimisation then
                           Ada.Text_IO.Put_Line ("S (K p) q: " &
                                                   SK.Images.Image
                                                   (Cells, E));
                        end if;
                        SK.Stack.Push (Cells, E);
                        return True;
                     end if;
                  end if;
               elsif K_Y then
                  Check_B (X, P, R, B_P_Q);
                  if Use_Dash_Combinators and B_P_Q then
                     SK.Stack.Push (Cells, E);
                     SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 3));
                     SK.Cells.Set_Car (Cells, Ts (1), Cd);
                     SK.Cells.Set_Cdr (Cells, Ts (1), P);
                     SK.Cells.Set_Car (Cells, Ts (2), Ts (1));
                     SK.Cells.Set_Cdr (Cells, Ts (2), R);
                     SK.Cells.Set_Car (Cells, Ts (3), Ts (2));
                     SK.Cells.Set_Cdr (Cells, Ts (3), Q);
                     SK.Stack.Drop (Cells, 1);
                     SK.Stack.Push (Cells, Ts (3));
                     if Debug_Optimisation then
                        Ada.Text_IO.Put_Line ("S (K p) (B q r): " &
                                                SK.Images.Image
                                                (Cells, Ts (3)));
                     end if;
                  else
                     SK.Cells.Set_Cdr (Cells, E, Q);
                     SK.Cells.Set_Car (Cells, SK.Cells.Car (Cells, E), C);
                     SK.Stack.Push (Cells, E);
                     if Debug_Optimisation then
                        Ada.Text_IO.Put_Line ("S p (K q): " &
                                                SK.Images.Image
                                                (Cells, E));
                     end if;
                  end if;
                  return True;
               else
                  Check_B (X, P, Q, B_P_Q);
                  if Use_Dash_Combinators and B_P_Q then
                     SK.Stack.Push (Cells, E);
                     SK.Cells.Allocate (Cells, O_Application, Ts (1 .. 3));
                     SK.Cells.Set_Car (Cells, Ts (1), Sd);
                     SK.Cells.Set_Cdr (Cells, Ts (1), P);
                     SK.Cells.Set_Car (Cells, Ts (2), Ts (1));
                     SK.Cells.Set_Cdr (Cells, Ts (2), Q);
                     SK.Cells.Set_Car (Cells, Ts (3), Ts (2));
                     SK.Cells.Set_Cdr (Cells, Ts (3), Y);
                     SK.Stack.Drop (Cells, 1);
                     SK.Stack.Push (Cells, Ts (3));
                     if Debug_Optimisation then
                        Ada.Text_IO.Put_Line ("S (B p q) r: original: " &
                                                SK.Images.Image
                                                (Cells, E));
                        Ada.Text_IO.Put_Line ("S (B p q) r: result:   " &
                                                SK.Images.Image
                                                (Cells, Ts (3)));
                     end if;
                     return True;
                  end if;
               end if;
            end if;

            SK.Stack.Push (Cells, E);
            Left_Changed := Optimise (SK.Cells.Car (Cells, E));
            Right_Changed := Optimise (SK.Cells.Cdr (Cells, E));

            Y := SK.Stack.Pop (Cells);
            X := SK.Stack.Pop (Cells);
            Z := SK.Stack.Pop (Cells);
            SK.Cells.Set_Car (Cells, Z, X);
            SK.Cells.Set_Cdr (Cells, Z, Y);
            SK.Stack.Push (Cells, Z);

            return Left_Changed or Right_Changed;

         else

            SK.Stack.Push (Cells, E);
            return False;

         end if;

      end Optimise;

      Changed : Boolean := False;

   begin
      Compile (Item);

      if False then
         loop
            Changed := Optimise (SK.Stack.Pop (Cells));
            exit when not Changed;
         end loop;
      end if;

      if Debug_Optimisation then
         Ada.Text_IO.Put_Line ("Result: " &
                                 SK.Images.Image
                                 (Cells, SK.Stack.Top (Cells)));
      end if;
      return SK.Stack.Pop (Cells);
   end Compile;

   ----------
   -- Link --
   ----------

   function Link
     (Cells : SK.Cells.Managed_Cells;
      Env   : SK.Environments.Environment;
      Item  : Object)
      return Object
   is
      procedure Do_Link (Item   : Object);

      -------------
      -- Do_Link --
      -------------

      procedure Do_Link (Item   : Object) is
         Result : Object;
      begin
         if Is_Symbol (Item) then

            Result := Environments.Get_Definition (Env, Item);

            if not SK.Environments.Is_Linked (Env, Item) then

               if Debug_Compiler then
                  Ada.Text_IO.Put_Line ("Linking: " &
                                        SK.Images.Image (Cells, Item) &
                                        " = " &
                                        SK.Images.Image (Cells, Result));
               end if;

               SK.Environments.Set_Linked (Env, Item);
               Do_Link (Result);
               SK.Stack.Pop (Cells, Result);
               SK.Environments.Replace_Definition (Env, Item, Result);
            end if;

            SK.Stack.Push (Cells, Result);

         elsif Is_Atomic (Item) then

            SK.Stack.Push (Cells, Item);

         else

            if Debug_Compiler then
               Ada.Text_IO.Put_Line ("Linking: " &
                                     SK.Images.Image (Cells, Item));
            end if;

            SK.Stack.Push (Cells, Item);
            Do_Link (SK.Cells.Car (Cells, SK.Stack.Get (Cells, 1)));
            Do_Link (SK.Cells.Cdr (Cells, SK.Stack.Get (Cells, 2)));
            declare
               Left, Right : Object;
            begin
               SK.Stack.Pop (Cells, Right);
               SK.Stack.Pop (Cells, Left);
               SK.Stack.Pop (Cells, Result);
               SK.Cells.Set_Car (Cells, Result, Left);
               SK.Cells.Set_Cdr (Cells, Result, Right);
               SK.Stack.Push (Cells, Result);
            end;

         end if;
      end Do_Link;

   begin
      if Debug_Compiler then
         Ada.Text_IO.Put_Line ("Linking: [" & Hex_Image (Item) & "] " &
                               SK.Images.Image (Cells, Item));
      end if;
      Do_Link (Item);
      return SK.Stack.Pop (Cells);
   end Link;

end SK.Compiler;
