with Ada.Text_IO;

package body SK.Functions.Primitives is

   function Evaluate_Eq
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Neq
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Int_First
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is (To_Object (-Max_Int - 1));

   function Evaluate_Int_Last
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is (To_Object (Max_Int));

   function Evaluate_Integer_Plus
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Integer_Minus
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Integer_Mult
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Integer_Div
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Integer_Mod
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Object_LT
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Object_LE
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Object_GT
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Object_GE
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   type Ordering is (LT, LE, GT, GE);

   function Evaluate_Object_Compare (Ord : Ordering;
                                     Args : Array_Of_Objects)
                                     return Object;

   function Evaluate_Putchar
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object;

   function Evaluate_Untyped_Identity
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is (Args (Args'First));

   function Evaluate_Fail
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is (raise Evaluation_Error with "#fail");

   function Evaluate_Undefined
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is (raise Evaluation_Error with "attempted to evaluate undefined");

   function Evaluate_Init_World
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is (Initial_World);

   ----------------
   -- Initialise --
   ----------------

   procedure Add_Primitives is
   begin
      Bind_Function ("eq?", 2, Evaluate_Eq'Access);
      Bind_Function ("#intEq", 2, Evaluate_Eq'Access);
      Bind_Function ("neq?", 2, Evaluate_Neq'Access);
      Bind_Function ("#intFirst", 0, Evaluate_Int_First'Access);
      Bind_Function ("#intLast", 0, Evaluate_Int_Last'Access);
      Bind_Function ("#intPlus", 2, Evaluate_Integer_Plus'Access);
      Bind_Function ("#intMinus", 2,
                    Evaluate_Integer_Minus'Access);
      Bind_Function ("#intMult", 2,
                    Evaluate_Integer_Mult'Access);
      Bind_Function ("#intDiv", 2,
                    Evaluate_Integer_Div'Access);
      Bind_Function ("#intMod", 2,
                    Evaluate_Integer_Mod'Access);
      Bind_Function ("#objGE", 2, Evaluate_Object_GE'Access);
      Bind_Function ("#objGT", 2, Evaluate_Object_GT'Access);
      Bind_Function ("#objLE", 2, Evaluate_Object_LE'Access);
      Bind_Function ("#objLT", 2, Evaluate_Object_LT'Access);
      Bind_Function ("#putchar", 2, Evaluate_Putchar'Access);
      Bind_Function ("#charPos", 1, Evaluate_Untyped_Identity'Access);
      Bind_Function ("#charVal", 1, Evaluate_Untyped_Identity'Access);
      Bind_Function ("#fail", 0, Evaluate_Fail'Access);
      Bind_Function ("#undefined", 0, Evaluate_Undefined'Access);
      Bind_Function ("#initWorld", 0, Evaluate_Init_World'Access);

   end Add_Primitives;

   -----------------
   -- Evaluate_Eq --
   -----------------

   function Evaluate_Eq
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin
      return To_Object (Args (1) = Args (2));
   end Evaluate_Eq;

   ---------------------------
   -- Evaluate_Integer_Div --
   ---------------------------

   function Evaluate_Integer_Div
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin

      Check (Args (1), "div", Is_Number'Access, "expected a number");
      Check (Args (2), "div", Is_Number'Access, "expected a number");

      return To_Object (Get_Integer (Args (1)) / Get_Integer (Args (2)));

   end Evaluate_Integer_Div;

   -----------------------------
   -- Evaluate_Integer_Minus --
   ----------------------------

   function Evaluate_Integer_Minus
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin

      Check (Args (1), "integer_minus", Is_Number'Access, "expected a number");
      Check (Args (2), "integer_minus", Is_Number'Access, "expected a number");

      return To_Object (Get_Integer (Args (1)) - Get_Integer (Args (2)));

   end Evaluate_Integer_Minus;

   ---------------------------
   -- Evaluate_Integer_Mod --
   ---------------------------

   function Evaluate_Integer_Mod
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin

      Check (Args (1), "integer_mod", Is_Number'Access, "expected a number");
      Check (Args (2), "integer_mod", Is_Number'Access, "expected a number");

      return To_Object (Get_Integer (Args (1)) mod Get_Integer (Args (2)));

   end Evaluate_Integer_Mod;

   ---------------------------
   -- Evaluate_Integer_Mult --
   ---------------------------

   function Evaluate_Integer_Mult
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin

      Check (Args (1), "integer_multiply",
             Is_Number'Access, "expected a number");
      Check (Args (2), "integer_multiply",
             Is_Number'Access, "expected a number");

      return To_Object (Get_Integer (Args (1)) * Get_Integer (Args (2)));

   end Evaluate_Integer_Mult;

   ---------------------------
   -- Evaluate_Integer_Plus --
   ---------------------------

   function Evaluate_Integer_Plus
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin

      Check (Args (1), "integer_plus", Is_Number'Access, "expected a number");
      Check (Args (2), "integer_plus", Is_Number'Access, "expected a number");

      return Args (1) + Args (2);

   end Evaluate_Integer_Plus;

   ------------------
   -- Evaluate_Neq --
   ------------------

   function Evaluate_Neq
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin
      return To_Object (Args (1) /= Args (2));
   end Evaluate_Neq;

   -----------------------------
   -- Evaluate_Object_Compare --
   -----------------------------

   function Evaluate_Object_Compare (Ord : Ordering;
                                     Args : Array_Of_Objects)
                                     return Object
   is

      Left   : constant Object := Args (1);
      Right  : constant Object := Args (2);
      Result : Boolean;

   begin

      Check (Left, "integer_compare", Is_Number'Access, "expected a number");
      Check (Right, "integer_compare", Is_Number'Access, "expected a number");

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

   function Evaluate_Object_GE
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin
      return Evaluate_Object_Compare (GE, Args);
   end Evaluate_Object_GE;

   ------------------------
   -- Evaluate_Object_GT --
   ------------------------

   function Evaluate_Object_GT
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin
      return Evaluate_Object_Compare (GT, Args);
   end Evaluate_Object_GT;

   ------------------------
   -- Evaluate_Object_LE --
   ------------------------

   function Evaluate_Object_LE
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin
      return Evaluate_Object_Compare (LE, Args);
   end Evaluate_Object_LE;

   ------------------------
   -- Evaluate_Object_LT --
   ------------------------

   function Evaluate_Object_LT
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
   begin
      return Evaluate_Object_Compare (LT, Args);
   end Evaluate_Object_LT;

   ----------------------
   -- Evaluate_Putchar --
   ----------------------

   function Evaluate_Putchar
     (Cells : SK.Cells.Managed_Cells;
      Args  : Array_Of_Objects)
      return Object
   is
      pragma Unreferenced (Cells);
      World       : constant Object := Args (2);
      Char_Object : constant Object := Args (1);
      Char_Val    : constant Integer := Get_Integer (Char_Object);
      Char        : constant Character := Character'Val (Char_Val);
   begin

      Ada.Text_IO.Put (Char);

      return Next_World (World);

   end Evaluate_Putchar;

end SK.Functions.Primitives;
