with Ada.Text_IO;

with SK.Cells;
with SK.Images;
with SK.Stack;

package body SK.Functions.Primitives is

   Debug_Primitives : constant Boolean := False;

   function Evaluate_Ifz (Cells : SK.Cells.Managed_Cells)
                           return Object;

   function Evaluate_Ifnz (Cells : SK.Cells.Managed_Cells)
                          return Object;

  function Evaluate_Eq (Cells : SK.Cells.Managed_Cells)
                        return Object;

   function Evaluate_Integer_Plus (Cells : SK.Cells.Managed_Cells)
                                   return Object;

   function Evaluate_Integer_Minus (Cells : SK.Cells.Managed_Cells)
                                    return Object;

   function Evaluate_Integer_Mult (Cells : SK.Cells.Managed_Cells)
                                    return Object;

   function Evaluate_Integer_Div (Cells : SK.Cells.Managed_Cells)
                                    return Object;

   function Evaluate_Integer_Mod (Cells : SK.Cells.Managed_Cells)
                                    return Object;

   function Evaluate_Object_LT (Cells : SK.Cells.Managed_Cells)
                                return Object;

   function Evaluate_Object_LE (Cells : SK.Cells.Managed_Cells)
                                return Object;

   function Evaluate_Object_GT (Cells : SK.Cells.Managed_Cells)
                                return Object;

   function Evaluate_Object_GE (Cells : SK.Cells.Managed_Cells)
                                return Object;

   type Ordering is (LT, LE, GT, GE);

   function Evaluate_Object_Compare (Ord : Ordering;
                                     Cells : SK.Cells.Managed_Cells)
                                     return Object;

   function Evaluate_Error (Cells : SK.Cells.Managed_Cells)
                           return Object;

   function Evaluate_Putchar (Cells : SK.Cells.Managed_Cells)
                             return Object;

   function Evaluate_Pick (Cells : SK.Cells.Managed_Cells)
                          return Object;

   function Evaluate_Combinator_I (Cells : SK.Cells.Managed_Cells)
                                  return Object;

   ----------------
   -- Initialise --
   ----------------

   procedure Add_Primitives (Env : SK.Environments.Environment) is
   begin
      Add_Internal_Function (Env, "ifz", 3, Evaluate_Ifz'Access);
      Add_Internal_Function (Env, "ifnz", 3, Evaluate_Ifnz'Access);
      Add_Internal_Function (Env, "eq?", 2, Evaluate_Eq'Access);
      Add_Internal_Function (Env, "#intPlus", 2, Evaluate_Integer_Plus'Access);
      Add_Internal_Function (Env, "#intMinus", 2,
                             Evaluate_Integer_Minus'Access);
      Add_Internal_Function (Env, "#intMult", 2,
                             Evaluate_Integer_Mult'Access);
      Add_Internal_Function (Env, "#intDiv", 2,
                             Evaluate_Integer_Div'Access);
      Add_Internal_Function (Env, "#intMod", 2,
                             Evaluate_Integer_Mod'Access);
      Add_Internal_Function (Env, "#objGE", 2, Evaluate_Object_GE'Access);
      Add_Internal_Function (Env, "#objGT", 2, Evaluate_Object_GT'Access);
      Add_Internal_Function (Env, "#objLE", 2, Evaluate_Object_LE'Access);
      Add_Internal_Function (Env, "#objLT", 2, Evaluate_Object_LT'Access);
      Add_Internal_Function (Env, "#error", 1, Evaluate_Error'Access);
      Add_Internal_Function (Env, "#putchar", 2, Evaluate_Putchar'Access);

      Add_Internal_Function (Env, "#I", 1, Evaluate_Combinator_I'Access);

      for I in 2 .. 15 loop
         Add_Internal_Function (Env,
                                "#pick" & Integer'Image (-I),
                                I + 1,
                                Evaluate_Pick'Access);
      end loop;

--        declare
--           E : Object;
--        begin
--           E := SK.Parser.Parse_String
        --  ("define Y (\f.(\x.f(x x)) (\x.f(x x)))");
--           E := Evaluate (E);
--        end;

   end Add_Primitives;

   ---------------------------
   -- Evaluate_Combinator_I --
   ---------------------------

   function Evaluate_Combinator_I (Cells : SK.Cells.Managed_Cells)
                                  return Object
   is
   begin
      return SK.Cells.Cdr (Cells, SK.Stack.Get (Cells, 1));
   end Evaluate_Combinator_I;

   -----------------
   -- Evaluate_Eq --
   -----------------

   function Evaluate_Eq (Cells : SK.Cells.Managed_Cells)
                        return Object
   is
      Left, Right : Object;
   begin

      Left  := Ground_Argument_Index (Cells, 1);
      Right := Ground_Argument_Index (Cells, 2);

      if Debug_Primitives then
         Ada.Text_IO.Put_Line ("eq? " & SK.Images.Image (Cells, Left) & " " &
                                 SK.Images.Image (Cells, Right));
      end if;

      return To_Object (Left = Right);
   end Evaluate_Eq;

   --------------------
   -- Evaluate_Error --
   --------------------

   function Evaluate_Error (Cells : SK.Cells.Managed_Cells)
                           return Object
   is
      Message : Object;
   begin
      Message := Ground_Argument_Index (Cells, 1);
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            SK.Images.Image (Cells, Message));
      raise Program_Error;
      return 0;
   end Evaluate_Error;

   -------------------
   -- Evaluate_Ifnz --
   -------------------

   function Evaluate_Ifnz (Cells : SK.Cells.Managed_Cells)
                           return Object
   is
   begin
      if Ground_Argument_Index (Cells, 1) /= 0 then
         return SK.Cells.Cdr (Cells, SK.Stack.Get (Cells, 2));
      else
         return SK.Cells.Cdr (Cells, SK.Stack.Get (Cells, 3));
      end if;
   end Evaluate_Ifnz;

   ------------------
   -- Evaluate_Ifz --
   ------------------

   function Evaluate_Ifz (Cells : SK.Cells.Managed_Cells)
                          return Object
   is
   begin
      if Ground_Argument_Index (Cells, 1) = 0 then
         return SK.Cells.Cdr (Cells, SK.Stack.Get (Cells, 2));
      else
         return SK.Cells.Cdr (Cells, SK.Stack.Get (Cells, 3));
      end if;
   end Evaluate_Ifz;

   ---------------------------
   -- Evaluate_Integer_Div --
   ---------------------------

   function Evaluate_Integer_Div (Cells : SK.Cells.Managed_Cells)
                                  return Object
   is

      Left, Right : Object;

   begin

      Left := Ground_Argument_Index (Cells, 1);
      Check (Cells, Left, "div", Is_Number'Access, "expected a number");

      Right := Ground_Argument_Index (Cells, 2);
      Check (Cells, Right, "div", Is_Number'Access, "expected a number");

      return To_Object (Get_Integer (Left) / Get_Integer (Right));

   end Evaluate_Integer_Div;

  -----------------------------
   -- Evaluate_Integer_Minus --
   ----------------------------

   function Evaluate_Integer_Minus (Cells : SK.Cells.Managed_Cells)
                                   return Object
   is

      Left, Right : Object;

   begin

      Left := Ground_Argument_Index (Cells, 1);
      Check (Cells, Left, "-", Is_Number'Access, "expected a number");

      Right := Ground_Argument_Index (Cells, 2);
      Check (Cells, Right, "-", Is_Number'Access, "expected a number");

      return Left - Right;

   end Evaluate_Integer_Minus;

   ---------------------------
   -- Evaluate_Integer_Mod --
   ---------------------------

   function Evaluate_Integer_Mod (Cells : SK.Cells.Managed_Cells)
                                  return Object
   is

      Left, Right : Object;

   begin

      Left := Ground_Argument_Index (Cells, 1);
      Check (Cells, Left, "mod", Is_Number'Access, "expected a number");

      Right := Ground_Argument_Index (Cells, 2);
      Check (Cells, Right, "mod", Is_Number'Access, "expected a number");

      return To_Object (Get_Integer (Left) mod Get_Integer (Right));

   end Evaluate_Integer_Mod;

   ---------------------------
   -- Evaluate_Integer_Mult --
   ---------------------------

   function Evaluate_Integer_Mult (Cells : SK.Cells.Managed_Cells)
                                  return Object
   is

      Left, Right : Object;

   begin

      Left := Ground_Argument_Index (Cells, 1);
      Check (Cells, Left, "*", Is_Number'Access, "expected a number");

      Right := Ground_Argument_Index (Cells, 2);
      Check (Cells, Right, "*", Is_Number'Access, "expected a number");

      return To_Object (Get_Integer (Left) * Get_Integer (Right));

   end Evaluate_Integer_Mult;

   ---------------------------
   -- Evaluate_Integer_Plus --
   ---------------------------

   function Evaluate_Integer_Plus (Cells : SK.Cells.Managed_Cells)
                                   return Object
   is

      Left, Right : Object;

   begin

      Left := Ground_Argument_Index (Cells, 1);
      Check (Cells, Left, "+", Is_Number'Access, "expected a number");

      Right := Ground_Argument_Index (Cells, 2);
      Check (Cells, Right, "+", Is_Number'Access, "expected a number");

      if Debug_Primitives then
         Ada.Text_IO.Put_Line ("+ " & SK.Images.Image (Cells, Left) & " " &
                                 SK.Images.Image (Cells, Right));
      end if;
      return Left + Right;

   end Evaluate_Integer_Plus;


   -----------------------------
   -- Evaluate_Object_Compare --
   -----------------------------

   function Evaluate_Object_Compare (Ord   : Ordering;
                                     Cells : SK.Cells.Managed_Cells)
                                     return Object
   is

      Left, Right : Object;
      Result : Boolean;

   begin

      Left := Ground_Argument_Index (Cells, 1);
      Check (Cells, Left, "#intCmp", Is_Number'Access, "expected a number");

      Right := Ground_Argument_Index (Cells, 2);
      Check (Cells, Right, "#intCmp", Is_Number'Access, "expected a number");

      case Ord is
         when LT =>
            Result := Left < Right;
         when LE =>
            Result := Left <= Right;
         when GT =>
            Result := Left > Right;
         when GE =>
            Result := Left >= Right;
      end case;

      return To_Object (Result);

   end Evaluate_Object_Compare;

   ------------------------
   -- Evaluate_Object_GE --
   ------------------------

   function Evaluate_Object_GE (Cells : SK.Cells.Managed_Cells)
                                return Object
   is
   begin
      return Evaluate_Object_Compare (GE, Cells);
   end Evaluate_Object_GE;

   ------------------------
   -- Evaluate_Object_GT --
   ------------------------

   function Evaluate_Object_GT (Cells : SK.Cells.Managed_Cells)
                                return Object
   is
   begin
      return Evaluate_Object_Compare (GT, Cells);
   end Evaluate_Object_GT;

   ------------------------
   -- Evaluate_Object_LE --
   ------------------------

   function Evaluate_Object_LE (Cells : SK.Cells.Managed_Cells)
                                return Object
   is
   begin
      return Evaluate_Object_Compare (LE, Cells);
   end Evaluate_Object_LE;

   ------------------------
   -- Evaluate_Object_LT --
   ------------------------

   function Evaluate_Object_LT (Cells : SK.Cells.Managed_Cells)
                                return Object
   is
   begin
      return Evaluate_Object_Compare (LT, Cells);
   end Evaluate_Object_LT;

   -------------------
   -- Evaluate_Pick --
   -------------------

   function Evaluate_Pick (Cells : SK.Cells.Managed_Cells)
                          return Object
   is
      Index_Object : constant Object := Ground_Argument_Index (Cells, 1);
      Index        : constant Natural := Get_Integer (Index_Object);
   begin
      return SK.Cells.Cdr (Cells, SK.Stack.Get (Cells, Index + 2));
   end Evaluate_Pick;

   ----------------------
   -- Evaluate_Putchar --
   ----------------------

   function Evaluate_Putchar (Cells : SK.Cells.Managed_Cells)
                             return Object
   is
      World       : constant Object := Ground_Argument_Index (Cells, 1);
      Char_Object : constant Object := Ground_Argument_Index (Cells, 2);
      Char_Val    : constant Integer := Get_Integer (Char_Object);
      Char        : constant Character := Character'Val (Char_Val);
   begin

      Ada.Text_IO.Put (Char);

      return World + 8;


--  --      return To_Object (Integer'(0));

--        SK.Cells.Allocate (Cells, O_Application, Ts);
--        SK.Cells.Set_Car (Cells, Ts (1), C);
--        SK.Cells.Set_Cdr (Cells, Ts (1), I);
--        SK.Cells.Set_Car (Cells, Ts (2), Ts (1));
--        SK.Cells.Set_Cdr (Cells, Ts (2), 0);

--        return Ts (2);
   end Evaluate_Putchar;

end SK.Functions.Primitives;
