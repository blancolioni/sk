with Ada.Exceptions;
with Ada.Text_IO;

with SK.Stack;

package body SK.Parser is

   Parse_Error : exception;
   Bad_Index   : Positive;

   procedure Do_Parse (Cells  : in     SK.Cells.Managed_Cells;
                       S      : in     String;
                       Index  : in out Positive);

   procedure Parse_Symbol (S      : in     String;
                           Index  : in out Positive;
                           Sym    :    out Object;
                           Simple : in     Boolean   := False);

   procedure Bad_Parse (Index   : Integer;
                        Message : String);

   procedure Expect (S     : in     String;
                     Index : in out Positive;
                     Char  : in     Character);

   procedure Skip_Spaces (S     : in     String;
                          Index : in out Positive);

   ---------------
   -- Bad_Parse --
   ---------------

   procedure Bad_Parse (Index   : Integer;
                        Message : String)
   is
   begin
      Bad_Index := Index;
      raise Parse_Error with Message;
   end Bad_Parse;

   --------------
   -- Do_Parse --
   --------------

   procedure Do_Parse (Cells  : in     SK.Cells.Managed_Cells;
                       S      : in     String;
                       Index  : in out Positive)
   is
      First : Boolean := True;
      Item  : Object;
   begin
      loop
         Skip_Spaces (S, Index);
         exit when Index > S'Last or else S (Index) = ')';

         case S (Index) is
            when '(' =>
               Index := Index + 1;
               Do_Parse (Cells, S, Index);
               Expect (S, Index, ')');
            when '\' =>
               Index := Index + 1;
               declare
                  Var    : Object;
                  Expr   : Object;
                  Result : Object;
               begin
                  Parse_Symbol (S, Index, Var, Simple => True);
                  Expect (S, Index, '.');
                  Do_Parse (Cells, S, Index);
                  SK.Cells.Allocate (Cells, O_Lambda, Result);
                  SK.Stack.Pop (Cells, Expr);
                  SK.Cells.Set_Car (Cells, Result, Var);
                  SK.Cells.Set_Cdr (Cells, Result, Expr);
                  SK.Stack.Push (Cells, Result);
               end;
            when '0' .. '9' =>
               declare
                  Value : Integer := 0;
               begin
                  while Index <= S'Last and then S (Index) in '0' .. '9' loop
                     Value := Value * 10 + Character'Pos (S (Index)) -
                       Character'Pos ('0');
                     Index := Index + 1;
                  end loop;
                  Item := To_Object (Value);
                  SK.Stack.Push (Cells, Item);
               end;
            when others =>
               Parse_Symbol (S, Index, Item);
               SK.Stack.Push (Cells, Item);
         end case;

         if First then
            First  := False;

         else
            declare
               Result, Left, Right : Object;
            begin
               SK.Cells.Allocate (Cells, O_Application, Result);
               SK.Stack.Pop (Cells, Right);
               SK.Stack.Pop (Cells, Left);
               SK.Cells.Set_Car (Cells, Result, Left);
               SK.Cells.Set_Cdr (Cells, Result, Right);
               SK.Stack.Push (Cells, Result);
            end;
         end if;
      end loop;

--        if Debug_Parser then
--           Ada.Text_IO.Put_Line ("Do_Parse: " & SK.Images.Image (Result));
--        end if;

   end Do_Parse;

   ------------
   -- Expect --
   ------------

   procedure Expect (S     : in     String;
                     Index : in out Positive;
                     Char  : in     Character)
   is
   begin
      Skip_Spaces (S, Index);
      if Index > S'Last or else S (Index) /= Char then
         Bad_Parse (Index, "missing '" & Char & "'");
      end if;
      Index := Index + 1;
   end Expect;

   ------------------
   -- Parse_String --
   ------------------

   function Parse_String (Cells : SK.Cells.Managed_Cells;
                          S     : String)
                          return Object
   is
      Index  : Positive := S'First;
      Result : Object   := Fail_Object;
   begin
      Do_Parse (Cells, S, Index);
      begin
         Skip_Spaces (S, Index);
         if Index <= S'Last then
            Bad_Parse (Index, "extra characters ignored");
         end if;
      end;

      SK.Stack.Pop (Cells, Result);
      return Result;

   exception
      when E : Parse_Error =>
         Ada.Text_IO.Put_Line ("Parse error");
         Ada.Text_IO.Put_Line (S);
         declare
            Dashes : constant String (1 .. Bad_Index - 1) := (others => '-');
         begin
            Ada.Text_IO.Put_Line (Dashes & "^");
         end;
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
         return Fail_Object;
   end Parse_String;

   ------------------
   -- Parse_Symbol --
   ------------------

   procedure Parse_Symbol (S      : in     String;
                           Index  : in out Positive;
                           Sym    :    out Object;
                           Simple : in     Boolean   := False)
   is
      function Symbol_Character (Ch : Character) return Boolean;

      ----------------------
      -- Symbol_Character --
      ----------------------

      function Symbol_Character (Ch : Character) return Boolean is
      begin
         return Ch /= ' ' and then Ch /= '(' and then Ch /= ')' and then
           (not Simple or else Ch /= '.');
      end Symbol_Character;

   begin
      Skip_Spaces (S, Index);
      if Index > S'Last or else not Symbol_Character (S (Index)) then
         Bad_Parse (Index, "missing symbol");
      end if;
      declare
         Start : constant Positive := Index;
      begin
         while Index <= S'Last and then Symbol_Character (S (Index)) loop
            Index := Index + 1;
         end loop;
         declare
            Sym_Name : constant String := S (Start .. Index - 1);
         begin
            if Sym_Name = "S" then
               Sym := SK.S;
            elsif Sym_Name = "K" then
               Sym := K;
            elsif Sym_Name = "I" then
               Sym := I;
            elsif Sym_Name = "B" then
               Sym := B;
            elsif Sym_Name = "C" then
               Sym := C;
            else
               Sym := Get_Symbol (S (Start .. Index - 1));
            end if;
         end;
      end;
   end Parse_Symbol;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces (S     : in     String;
                          Index : in out Positive)
   is
   begin
      while Index <= S'Last and then S (Index) = ' ' loop
         Index := Index + 1;
      end loop;
   end Skip_Spaces;

end SK.Parser;
