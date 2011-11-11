with Ada.Text_IO;

with SK.Debug;
with SK.Debug_Class;

with SK.Images;
with SK.Interfaces;

package body SK.Pattern_Matching is

   function Debug_Pattern_Matching return Boolean;

   procedure Do_Match
     (Pattern   : in     SK.Object;
      Expr      : in out SK.Object;
      Bindings  : in out SK.Array_Of_Objects;
      Index     : in out Positive;
      Success   :    out Boolean;
      Ground    : in     Boolean);

   ----------------------------
   -- Debug_Pattern_Matching --
   ----------------------------

   function Debug_Pattern_Matching return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug_Class.Pattern_Matching);
   end Debug_Pattern_Matching;

   --------------
   -- Do_Match --
   --------------

   procedure Do_Match
     (Pattern    : in     SK.Object;
      Eval_Stack : in     SK.Stacks.
      Expr_Index : in     Positive;
      Bindings   : in out SK.Array_Of_Objects;
      Index      : in out Positive;
      Success    :    out Boolean;
      Ground     : in     Boolean)
   is
      use SK;
      use SK.Interfaces;
      Ground_Expr : Object;
      Pattern_Value : constant Object := Evaluate (Pattern);
   begin

      if Debug_Pattern_Matching then
         Ada.Text_IO.Put_Line (Images.Image (Expr));
      end if;

      if Pattern_Value = Unbound_Object or
        Pattern_Value = Dont_Care_Object
      then
         if Pattern_Value = Unbound_Object then
            if Ground then
               Bindings (Index) := Expr;
            else
               Bindings (Index) := Pass_Argument (Expr);
            end if;
            if Debug_Pattern_Matching then
               Ada.Text_IO.Put_Line
                 ("Binding at" &
                  Integer'Image (Index) & ": " &
                  SK.Images.Image (Bindings (Index)));
            end if;
            Index := Index + 1;
         end if;
         Success := True;
      else
         if not Ground then
            Ground_Expr := Ground_Argument (Expr);
         else
            Ground_Expr := Expr;
         end if;

         if Debug_Pattern_Matching then
            Ada.Text_IO.Put_Line ("Matching " &
                                  SK.Images.Image (Pattern_Value) &
                                  " to " &
                                  SK.Images.Image (Ground_Expr));
         end if;

         if Is_Immediate (Pattern_Value) and then
           Pattern_Value = Ground_Expr
         then
            Success := True;
         elsif Is_Pair (Pattern_Value) and Is_Pair (Ground_Expr) then

            if Debug_Pattern_Matching then
               Ada.Text_IO.Put_Line ("Matching pairs");
            end if;

            declare
               P, E : Object;
            begin
               P := Pattern_Value;
               E := Ground_Expr;
               while Is_Pair (P) and Is_Pair (E) loop
                  Ground_Expr := Evaluate (Get_Left (E));
                  Do_Match (Get_Left (P), Ground_Expr,
                            Bindings, Index, Success, True);
                  exit when not Success;
                  P := Get_Right (P);
                  E := Evaluate (Get_Right (E));
               end loop;
               if Success then
                  Do_Match (P, E, Bindings, Index, Success, True);
               end if;
            end;
         else
            Success := False;
         end if;
      end if;
   end Do_Match;

   -----------
   -- Match --
   -----------

   procedure Match (Pattern    : in     SK.Object;
                    Eval_Stack : in     SK.Stacks.Evaluation_Stack;
                    Expr_Index : in     Positive;
                    Bindings   : in out SK.Array_Of_Objects;
                    Success    :    out Boolean)
   is
      Binding_Index : Positive := 1;
   begin
      Do_Match (Pattern, Eval_Stack, Expr_Index, Bindings, Binding_Index,
                Success, False);
   end Match;

end SK.Pattern_Matching;

