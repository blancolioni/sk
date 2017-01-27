with Ada.Text_IO;

with SK.Debug;
with SK.Objects.Symbols;

package body SK.Compiler is

   function Debug_Compiler return Boolean;
   function Debug_Optimisation return Boolean;

   -------------
   -- Compile --
   -------------

   function Compile
     (Store : in out SK.Objects.Object_Store'Class;
      Item  : SK.Objects.Object)
      return SK.Objects.Object
   is
      use SK.Objects, SK.Objects.Symbols;

      procedure Compile (E : Object);

      procedure Abstract_Expr (X : Object;
                               E : Object);

      -------------------
      -- Abstract_Expr --
      -------------------

      procedure Abstract_Expr
        (X : Object;
         E : Object)
      is
      begin
         if X /= Dont_Care and then Get_Name (X) = "_" then
            Abstract_Expr (Dont_Care, E);
         elsif E = X then
            Store.Push (I);
         elsif Is_Application (E) then
            if Is_Application (Store.Left (E))
              and then Store.Left (Store.Left (E)) = Lambda
            then
               Abstract_Expr
                 (Store.Right (Store.Left (E)), Store.Right (E));
               Abstract_Expr
                 (X, Store.Pop);
            else
               Store.Push_Secondary (E);
               Store.Push (S);
               Abstract_Expr (X, Store.Left (Store.Top_Secondary));
               Store.Apply;
               Abstract_Expr (X, Store.Right (Store.Top_Secondary));
               Store.Apply;
               Store.Pop_Secondary;
               Store.Push_Secondary (Store.Pop);

               declare
                  function Position return Object_Cursor
                  is (Store.Top_Secondary_Cursor);
                  Left     : constant Object :=
                               Position.Left.Right.Value;
                  Right    : constant Object :=
                               Position.Right.Value;
               begin
                  if Is_Application (Left)
                    and then Store.Left (Left) = K
                  then
                     if Right = I then
                        --  S (K x) I -> x
                        Store.Push (Store.Right (Left));
                        if Debug_Compiler then
                           Ada.Text_IO.Put_Line
                             ("opt: " & Store.Show (Store.Top_Secondary)
                              & " -> "
                              & Store.Show (Store.Top));
                        end if;
                     elsif Is_Application (Right)
                       and then Store.Left (Right) = K
                     then
                        --  S (K x) (K y) -> K (x y)
                        Store.Push (K);
                        Store.Push
                          (Position.Left.Right.Right);
                        Store.Push
                          (Position.Right.Right);
                        Store.Apply;
                        Store.Apply;
                        if Debug_Compiler then
                           Ada.Text_IO.Put_Line
                             ("opt: " & Store.Show (Store.Top_Secondary)
                              & " -> "
                              & Store.Show (Store.Top));
                        end if;
                     else
                        --  S (K x) y -> B x y
                        Store.Push (B);
                        Store.Push (Position.Left.Right.Right);
                        Store.Apply;
                        Store.Push (Position.Right);
                        Store.Apply;
                        if Debug_Compiler then
                           Ada.Text_IO.Put_Line
                             ("opt: " & Store.Show (Store.Top_Secondary)
                              & " -> "
                              & Store.Show (Store.Top));
                        end if;
                     end if;
                  elsif Position.Right.Left.Value = K then
                     Store.Push (C);
                     Store.Push (Position.Left.Right);
                     Store.Apply;
                     Store.Push (Position.Right.Right);
                     Store.Apply;
                     if Debug_Compiler then
                        Ada.Text_IO.Put_Line
                          ("opt: " & Store.Show (Store.Top_Secondary)
                           & " -> "
                           & Store.Show (Store.Top));
                     end if;
                  else
                     Store.Push (S);
                     Store.Push (Position.Left.Right);
                     Store.Apply;
                     Store.Push (Position.Right);
                     Store.Apply;
                     if Debug_Compiler then
                        Ada.Text_IO.Put_Line
                          ("no opt: " & Store.Show (Store.Top));
                     end if;
                  end if;
               end;

               Store.Pop_Secondary;

               if Debug_Optimisation then
                  Ada.Text_IO.Put_Line (Store.Show (Store.Top));
               end if;
            end if;
         else
            Store.Push_Secondary (E);
            Store.Push (K);
            Store.Push (Object'(Store.Top_Secondary));
            Store.Pop_Secondary;
            Store.Apply;
         end if;
      end Abstract_Expr;

      -------------
      -- Compile --
      -------------

      procedure Compile (E : Object) is
      begin
         if Is_Application (E) then
            if Is_Application (Store.Left (E))
              and then Store.Left (Store.Left (E)) = Lambda
            then
               Abstract_Expr
                 (Store.Right (Store.Left (E)),
                  Store.Right (E));
            else
               Store.Push_Secondary (E);
               Compile (Store.Left (Store.Top_Secondary));
               Compile (Store.Right (Store.Top_Secondary));
               Store.Apply;
               Store.Pop_Secondary;
            end if;
         else
            Store.Push (E);
         end if;
      end Compile;

   begin

      if Debug_Compiler then
         Ada.Text_IO.Put_Line ("Compile: " & Store.Show (Item));
      end if;

      Compile (Item);

      if Debug_Compiler then
         Ada.Text_IO.Put_Line ("Result: " & Store.Show (Store.Top));
      end if;
      return Store.Pop;

   exception
      when others =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "error while compiling " & Store.Show (Item));
         raise;

   end Compile;

   --------------------
   -- Debug_Compiler --
   --------------------

   function Debug_Compiler return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug.Compiler);
   end Debug_Compiler;

   ------------------------
   -- Debug_Optimisation --
   ------------------------

   function Debug_Optimisation return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug.Optimisation);
   end Debug_Optimisation;

end SK.Compiler;
