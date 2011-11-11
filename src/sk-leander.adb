with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

with SK.Interfaces;

package body SK.Leander is

   function Evaluate_Prim_Lit_Char (Args : SK.Array_Of_Objects)
                                   return SK.Object;

   function Evaluate_Show_Int (Args : SK.Array_Of_Objects)
                               return SK.Object;

   function Evaluate_Seq (Args : SK.Array_Of_Objects)
                         return SK.Object;

   function Evaluate_Putchar (Args : SK.Array_Of_Objects)
                             return SK.Object;

   function Evaluate_Getchar (Args : SK.Array_Of_Objects)
                             return SK.Object;

   function Evaluate_IO_Join (Args : SK.Array_Of_Objects)
                             return SK.Object;

   ----------------------------------------
   -- Create_Primitive_Leander_Functions --
   ----------------------------------------

   procedure Create_Primitive_Leander_Functions is
   begin

      SK.Interfaces.New_Function ("Prelude.primShowInt", 1,
                                  Evaluate_Show_Int'Access,
                                  True);

      SK.Interfaces.New_Function ("Prelude.seq", 2,
                                  Evaluate_Seq'Access,
                                  False);

      SK.Interfaces.New_Function ("Prelude.primLitChar", 1,
                                  Evaluate_Prim_Lit_Char'Access,
                                  True);

      SK.Interfaces.New_Function ("Prelude.primPutChar", 2,
                                  Evaluate_Putchar'Access,
                                  True);

      SK.Interfaces.New_Function ("Prelude.primGetChar", 1,
                                  Evaluate_Getchar'Access,
                                  True);

      SK.Interfaces.New_Function ("Prelude.primIOJoin", 2,
                                  Evaluate_IO_Join'Access,
                                  False);

   end Create_Primitive_Leander_Functions;

   ----------------------
   -- Evaluate_Getchar --
   ----------------------

   function Evaluate_Getchar (Args : SK.Array_Of_Objects)
                             return SK.Object
   is
      pragma Unreferenced (Args);
      use Ada.Text_IO;
   begin
      if End_Of_Line then
         Skip_Line;
         return To_Object (Integer'(10));
      else
         declare
            Ch : Character;
         begin
            Get (Ch);
            return To_Object (Integer'(Character'Pos (Ch)));
         end;
      end if;
   end Evaluate_Getchar;

   ----------------------
   -- Evaluate_IO_Join --
   ----------------------

   function Evaluate_IO_Join (Args : SK.Array_Of_Objects)
                             return SK.Object
   is
      Left : constant SK.Object :=
        SK.Interfaces.Ground_Argument (Args (Args'First));
   begin
      declare
         Join : constant SK.Object :=
           SK.Apply (SK.Interfaces.Pass_Argument (Args (Args'First + 1)),
                     Left);
         Result : constant SK.Object := SK.Evaluate (Join);
      begin
         return Result;
      end;
   end Evaluate_IO_Join;

   ----------------------------
   -- Evaluate_Prim_Lit_Char --
   ----------------------------

   function Evaluate_Prim_Lit_Char (Args : SK.Array_Of_Objects)
                                   return SK.Object
   is
      function Char_To_String return String;
      --  Internal conversion to String, which the main function
      --  converts to a list

      --------------------
      -- Char_To_String --
      --------------------

      function Char_To_String return String is
         use Ada.Characters.Handling;
         use Ada.Characters.Latin_1;
         Ch_Index : constant Integer   := SK.Get_Integer (Args (Args'First));
         Ch       : constant Character := Character'Val (Ch_Index);
      begin
         if Is_Graphic (Ch) then
            return (1 => Ch);
         elsif Ch = BEL then
            return "\a";
         elsif Ch = BS then
            return "\b";
         elsif Ch = HT then
            return "\t";
         elsif Ch = LF then
            return "\n";
         elsif Ch = VT then
            return "\v";
         elsif Ch = FF then
            return "\f";
         elsif Ch = CR then
            return "\r";
         else
            declare
               Result : String := Integer'Image (Ch_Index);
            begin
               Result (Result'First) := '\';
               return Result;
            end;
         end if;
      end Char_To_String;

   begin
      return SK.String_To_List (Char_To_String);
   end Evaluate_Prim_Lit_Char;

   ----------------------
   -- Evaluate_Putchar --
   ----------------------

   function Evaluate_Putchar (Args : SK.Array_Of_Objects)
                             return SK.Object
   is
   begin
      Ada.Text_IO.Put (Character'Val (Get_Integer (Args (Args'First + 1))));
      return SK.Null_Object;
   end Evaluate_Putchar;

   ------------------
   -- Evaluate_Seq --
   ------------------

   function Evaluate_Seq (Args : SK.Array_Of_Objects)
                         return SK.Object
   is
      Left : constant SK.Object :=
        SK.Interfaces.Ground_Argument (Args (Args'First));
      pragma Unreferenced (Left);
   begin
      return SK.Interfaces.Pass_Argument (Args (Args'First + 1));
   end Evaluate_Seq;

   -----------------------
   -- Evaluate_Show_Int --
   -----------------------

   function Evaluate_Show_Int (Args : SK.Array_Of_Objects)
                               return SK.Object
   is
      Image : constant String :=
                Integer'Image (SK.Get_Integer (Args (Args'First)));
   begin
      if Image (Image'First) = ' ' then
         return SK.String_To_List (Image (Image'First + 1 .. Image'Last));
      else
         return SK.String_To_List (Image);
      end if;
   end Evaluate_Show_Int;

end SK.Leander;
