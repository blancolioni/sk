with Ada.Text_IO;

with SK.Images;

with SK.Lambdas;

with SK.Debug;
with SK.Debug_Class;


package body SK.Abstraction is

   procedure Trace_Abstraction (Cells   : SK.Cells.Managed_Cells;
                                Message : String;
                                Item    : Object);

   procedure Trace_Compaction (Cells   : SK.Cells.Managed_Cells;
                               Message : String;
                               Item    : Object);

   procedure Trace_Optimisation (Cells   : SK.Cells.Managed_Cells;
                                 Message : String;
                                 Item    : Object);

   function Abstract_Expr (Cells    : SK.Cells.Managed_Cells;
                           Variable : Object;
                           Expr     : Object)
     return Object;

   function Substitute (Cells       : SK.Cells.Managed_Cells;
                        Expr        : Object;
                        Variable    : Object;
                        Replacement : Object)
                       return Object;

   function Compact_Lambdas (Cells   : SK.Cells.Managed_Cells;
                             E       : Object)
                             return Object;
   --  Transform \x.E x into E when x does not occur free in E

   -------------------
   -- Abstract_Expr --
   -------------------

   function Abstract_Expr (Cells    : SK.Cells.Managed_Cells;
                           Variable : Object;
                           Expr     : Object)
                          return Object
   is
      Result : Object;

      function Apply (Left, Right : Object) return Object;


      -----------
      -- Apply --
      -----------

      function Apply (Left, Right : Object) return Object is
      begin
         return SK.Cells.Alloc_Application (Cells, Left, Right);
      end Apply;

   begin
      Trace_Abstraction (Cells,
                         "Abstracting '" & SK.Images.Image (Variable) &
                         "' from: ", Expr);

      if Is_Atomic (Expr) then
         if Equal (Variable, Expr) then
            Result := I;
         else
            Result := SK.Cells.Alloc_Application (Cells, K, Expr);
         end if;
      elsif Is_Pair (Expr) then
         --  preloaded pairs are treated like constants,
         --  so don't put anything in them that needs to be evaluated
         Result := Apply (K, Expr);
      elsif Is_Lambda (Expr) then
         declare
            V : constant Object := SK.Lambdas.Get_Variable (Cells, Expr);
            E : constant Object :=
                  Abstract_Expr (Cells, V, SK.Lambdas.Get_Body (Cells, Expr));
         begin
            if V /= Variable then
               Result := Abstract_Expr (Cells, Variable, E);
            else
               Result := E;
            end if;
         end;
      elsif Is_Application (Expr) then
         if False and Is_Lambda (SK.Cells.Car (Cells, Expr)) then
            --  That lambda is already applied!
            Result :=
              Abstract_Expr
                (Cells,
                 Variable,
                 Substitute
                   (Cells,
                    SK.Lambdas.Get_Body (Cells,
                                         SK.Cells.Car (Cells, Expr)),
                    SK.Lambdas.Get_Variable
                      (Cells,
                       SK.Cells.Car (Cells, Expr)),
                    SK.Cells.Cdr (Cells, Expr)));
         else
            Result :=
              Apply (Apply (S, Abstract_Expr (Cells, Variable,
                SK.Cells.Car (Cells, Expr))),
                Abstract_Expr (Cells, Variable, SK.Cells.Cdr (Cells, Expr)));
         end if;
      else
         Result := Expr;
      end if;


      Trace_Abstraction (Cells, "Returning: ", Result);
      return Result;

   end Abstract_Expr;

   -------------------------
   -- Abstract_Expression --
   -------------------------

   function Abstract_Expression (Cells   : SK.Cells.Managed_Cells;
                                 E       : Object)
                                 return Object
   is
      Result : Object := E;
   begin

      Result := Compact_Lambdas (Cells, Result);
      if Is_Application (Result) then
         if Is_Lambda (SK.Cells.Car (Cells, Result)) then
            Result :=
              Abstract_Expression
                (Cells,
                 Substitute
                   (Cells,
                    SK.Lambdas.Get_Body (Cells, SK.Cells.Car (Cells, Result)),
                    SK.Lambdas.Get_Variable (Cells,
                      SK.Cells.Car (Cells, Result)),
                    SK.Cells.Cdr (Cells, Result)));
         else
            Result :=
              SK.Cells.Alloc_Application
                (Cells,
                 Abstract_Expression (Cells, SK.Cells.Car (Cells, Result)),
                 Abstract_Expression (Cells, SK.Cells.Cdr (Cells, Result)));
         end if;
      elsif Is_Lambda (Result) then
         Result :=
           Abstract_Expr
             (Cells,
              SK.Lambdas.Get_Variable (Cells, Result),
              SK.Lambdas.Get_Body (Cells, Result));
      end if;

      Optimise (Cells, Result);

      Trace_Abstraction (Cells, "Returning: ", Result);

      return Result;
   end Abstract_Expression;

   ---------------------
   -- Compact_Lambdas --
   ---------------------

   function Compact_Lambdas (Cells   : SK.Cells.Managed_Cells;
                             E       : Object)
                             return Object
   is
      function Free (Variable : Object;
                     Expr     : Object)
                    return Boolean;
      --  return True if Variable occurs free in Expr

      ----------
      -- Free --
      ----------

      function Free (Variable : Object;
                     Expr     : Object)
                    return Boolean
      is
      begin
         if Is_Lambda (Expr) then
            if SK.Lambdas.Get_Variable (Cells, Expr) = Variable then
               return False;
            else
               return Free (Variable,
                            SK.Lambdas.Get_Body (Cells, Expr));
            end if;
         elsif Is_Application (Expr) then
            return Free (Variable, SK.Cells.Car (Cells, Expr)) or
              Free (Variable, SK.Cells.Cdr (Cells, Expr));
         elsif Expr = Variable then
            return True;
         else
            return False;
         end if;
      end Free;

      Result : Object;

   begin
      if Is_Lambda (E) then
         declare
            New_Body : constant Object :=
                         Compact_Lambdas (Cells,
                                          SK.Lambdas.Get_Body (Cells, E));
         begin
            if Is_Application (New_Body) and then
              (SK.Cells.Car (Cells, New_Body) =
                 SK.Lambdas.Get_Variable (Cells, E) and
               not Free (SK.Lambdas.Get_Variable (Cells, E),
                         SK.Cells.Car (Cells, New_Body)))
            then
               if SK.Debug.Enabled (SK.Debug_Class.Compaction) then
                  Trace_Compaction (Cells, "Compacting: ", E);
                  Trace_Compaction (Cells, "        to: ",
                                    SK.Cells.Car (Cells, New_Body));
               end if;
               Result := SK.Cells.Car (Cells, New_Body);
            else
               Result := SK.Lambdas.Lambda (Cells,
                                            SK.Lambdas.Get_Variable (Cells, E),
                                            New_Body);
            end if;
         end;
      elsif Is_Application (E) then
         Result :=
           SK.Cells.Alloc_Application
             (Cells,
              Compact_Lambdas (Cells, SK.Cells.Car (Cells, E)),
              Compact_Lambdas (Cells, SK.Cells.Cdr (Cells, E)));
      else
         Result := E;
      end if;

      return Result;

   end Compact_Lambdas;


   --------------
   -- Optimise --
   --------------

   procedure Optimise (Cells      : in     SK.Cells.Managed_Cells;
                       Expression : in out Object)
   is
      function Get_Left (Item : Object) return Object;
      function Get_Right (Item : Object) return Object;
      procedure Set_Left (Item : Object;
                          To   : Object);
      procedure Set_Right (Item : Object;
                           To   : Object);
      function Apply (Left, Right : Object) return Object;

      procedure Optimise (Expr : in out Object);

      -----------
      -- Apply --
      -----------

      function Apply (Left, Right : Object) return Object is
      begin
         return SK.Cells.Alloc_Application (Cells, Left, Right);
      end Apply;

      --------------
      -- Get_Left --
      --------------

      function Get_Left (Item : Object) return Object is
      begin
         return SK.Cells.Car (Cells, Item);
      end Get_Left;

      ---------------
      -- Get_Right --
      ---------------

      function Get_Right (Item : Object) return Object is
      begin
         return SK.Cells.Cdr (Cells, Item);
      end Get_Right;

     --------------
      -- Optimise --
      --------------

      procedure Optimise (Expr : in out Object) is

         Left, Right : Object;
         Changed     : Boolean := True;


      begin

         if not Is_Application (Expr) then
            return;
         end if;

         loop

            Trace_Optimisation (Cells, "Optimising: ", Expr);

            Left := Get_Left (Expr); Optimise (Left);
            Right := Get_Right (Expr); Optimise (Right);

            if (Is_Application (Right) and then
                  Get_Left (Right) = K) and
              (Is_Application (Left) and then
                 Get_Left (Left) = S)
            then
               if Is_Application (Get_Right (Left)) and then
                 Get_Left (Get_Right (Left)) = K
               then
                  --  S (K a) (K b) => K (a b)
                  Set_Left (Expr, K);
                  Set_Right (Expr, Apply (Get_Right (Get_Right (Left)),
                    Get_Right (Right)));
               else
                  --  S a (K b) => C a b
                  Set_Right (Expr, Get_Right (Right));
                  Set_Left (Expr, Apply (C, Get_Right (Left)));
               end if;
               Changed := True;
            elsif Is_Application (Left) and then
              (Get_Left (Left) = S and
                 (Is_Application (Get_Right (Left)) and then
                    Get_Left (Get_Right (Left)) = K))
            then
               if Right = I then
                  --  S (K a) I => a
                  Expr := Get_Right (Get_Right (Left));
               else
                  --  S (K a) b => B a b
                  Set_Right (Expr, Right);
                  Set_Left (Expr, Apply (B, Get_Right (Get_Right (Left))));
               end if;
               Changed := True;
            else
               Set_Left (Expr, Left);
               Set_Right (Expr, Right);
               Changed := False;
            end if;

            exit when not Changed or not Is_Application (Expr);

         end loop;

         Trace_Optimisation (Cells, "Optimised: ", Expr);

      end Optimise;

      --------------
      -- Set_Left --
      --------------

      procedure Set_Left (Item : Object;
                          To   : Object)
      is
      begin
         SK.Cells.Set_Car (Cells, Item, To);
      end Set_Left;

      ---------------
      -- Set_Right --
      ---------------

      procedure Set_Right (Item : Object;
                           To   : Object)
      is
      begin
         SK.Cells.Set_Cdr (Cells, Item, To);
      end Set_Right;

   begin

      Optimise (Expression);

   end Optimise;

   ----------------
   -- Substitute --
   ----------------

   function Substitute (Cells       : SK.Cells.Managed_Cells;
                        Expr        : Object;
                        Variable    : Object;
                        Replacement : Object)
                       return Object
   is
   begin
      if Is_Atomic (Expr) then
         if Equal (Variable, Expr) then
            return Replacement;
         else
            return Expr;
         end if;
      elsif Is_Lambda (Expr) then
         if SK.Lambdas.Get_Variable (Cells, Expr) = Variable then
            return Expr;
         else
            return SK.Lambdas.Lambda
              (Cells,
               SK.Lambdas.Get_Variable (Cells, Expr),
               Substitute (Cells,
                 SK.Lambdas.Get_Body (Cells, Expr), Variable,
                 Replacement));
         end if;
      elsif Is_Application (Expr) then
         return SK.Cells.Alloc_Application
           (Cells,
            Substitute (Cells, SK.Cells.Car (Cells, Expr),
              Variable, Replacement),
             Substitute (Cells, SK.Cells.Cdr (Cells, Expr),
              Variable, Replacement));
      else
         return Expr;
      end if;
   end Substitute;

   -----------------------
   -- Trace_Abstraction --
   -----------------------

   procedure Trace_Abstraction (Cells   : SK.Cells.Managed_Cells;
                                Message : String;
                                Item    : Object)
   is
   begin
      if SK.Debug.Enabled (SK.Debug_Class.Abstraction) then
         Ada.Text_IO.Put_Line (Message & SK.Images.Image (Item));
      end if;
   end Trace_Abstraction;

   ----------------------
   -- Trace_Compaction --
   ----------------------

   procedure Trace_Compaction (Cells   : SK.Cells.Managed_Cells;
                               Message : String;
                               Item    : Object)
   is
   begin
      if SK.Debug.Enabled (SK.Debug_Class.Compaction) then
         Ada.Text_IO.Put_Line (Message & SK.Images.Image (Item));
      end if;
   end Trace_Compaction;

   ------------------------
   -- Trace_Optimisation --
   ------------------------

   procedure Trace_Optimisation (Cells   : SK.Cells.Managed_Cells;
                                 Message : String;
                                 Item    : Object)
   is
   begin
      if SK.Debug.Enabled (SK.Debug_Class.Optimisation) then
         Ada.Text_IO.Put_Line (Message & SK.Images.Image (Item));
      end if;
   end Trace_Optimisation;

end SK.Abstraction;

