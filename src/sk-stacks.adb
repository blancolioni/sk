with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

with SK.Images;

with SK.Debug;
with SK.Debug_Class;

package body SK.Stacks is

   use Array_Of_Stacked_Objects;

   function Debug_Stacks return Boolean;

   Max_Stack_Depth : Natural := 0;

   procedure Free is
      new Ada.Unchecked_Deallocation (Evaluation_Stack_Record,
                                      Evaluation_Stack);
   --------------------
   -- Clear_Boundary --
   --------------------

   procedure Clear_Boundary (Stack : in out Evaluation_Stack) is
   begin
      if Stack.Last_Boundary /= 0 then
         Drop (Stack.Stack, Stack.Top - Stack.Last_Boundary + 1);
         Stack.Top := Stack.Last_Boundary - 1;
         Stack.Last_Boundary := 0;
         for I in reverse 1 .. Stack.Top loop
            if Get (Stack.Stack, I) = Boundary_Object then
               Stack.Last_Boundary := I;
               exit;
            end if;
         end loop;
         Stack.Boundaries := Stack.Boundaries - 1;
      else
         raise Stack_Error;
      end if;
   end Clear_Boundary;

   -----------------
   -- Close_Stack --
   -----------------

   procedure Close_Stack (Stack : in out Evaluation_Stack) is
   begin
      Free (Stack);
   end Close_Stack;

   ------------------
   -- Debug_Stacks --
   ------------------

   function Debug_Stacks return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug_Class.Stacks);
   end Debug_Stacks;

   ----------
   -- Drop --
   ----------

   procedure Drop (Stack   : in Evaluation_Stack;
                   Count   : in Natural          := 1)
   is
      Tmp : Object;
   begin
      for I in 1 .. Count loop
         Pop (Stack, Tmp);
      end loop;
   end Drop;

   ----------------
   -- Dump_Stack --
   ----------------

   procedure Dump_Stack (Stack          : Evaluation_Stack;
                         Cross_Boundary : Boolean)
   is
   begin

      for Index in reverse 1 .. Stack.Top loop
         if Get (Stack.Stack, Index) = Boundary_Object then
            exit when not Cross_Boundary;
            Ada.Text_IO.Put_Line (Index'Img & " ---- boundary ----");
         else
            Ada.Text_IO.Put_Line (Index'Img & ": " &
                                  SK.Images.Image (Get (Stack, Index)));
            Ada.Text_IO.Put_Line (Index'Img & ": " &
                                  SK.Images.Low_Level_Image
                                  (Get (Stack, Index)));
         end if;
      end loop;
   end Dump_Stack;

   ---------
   -- Get --
   ---------

   function Get (From_Stack : Evaluation_Stack;
                 Index      : Positive)
                return Object
   is
   begin
      return Get (From_Stack.Stack, From_Stack.Top - Index + 1);
   end Get;

   ------------------
   -- Get_Contents --
   ------------------

   function Get_Contents (Stack : Evaluation_Stack)
                         return Array_Of_Objects
   is
      Start  : constant Positive := Stack.Last_Boundary + 1;
      Count  : constant Natural  := Stack.Top - Start + 1;
      Result : Array_Of_Objects (1 .. Count);
   begin
      for I in 1 .. Count loop
         Result (I) := Get (Stack.Stack, I + Start - 1);
      end loop;
      return Result;
   end Get_Contents;

   -------------------------
   -- Get_Max_Stack_Depth --
   -------------------------

   function Get_Max_Stack_Depth return Natural is
   begin
      return Max_Stack_Depth;
   end Get_Max_Stack_Depth;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Stack : Evaluation_Stack) return Boolean is
   begin
      return Stack.Top = 0 or else Get (Stack, Stack.Top) = Boundary_Object;
   end Is_Empty;

   ---------------
   -- New_Stack --
   ---------------

   function New_Stack return Evaluation_Stack is
   begin
      return new Evaluation_Stack_Record;
   end New_Stack;

   -------------
   -- Operate --
   -------------

   procedure Operate (Stack          : in Evaluation_Stack;
                      Operator       : in Stack_Operator;
                      Cross_Boundary : in Boolean)
   is
   begin
      for Index in reverse 1 .. Stack.Top loop
         if Get (Stack.Stack, Index) = Boundary_Object then
            exit when not Cross_Boundary;
         else
            if Debug_Stacks then
               Ada.Text_IO.Put_Line ("operating on: " &
                                     SK.Images.Image
                                     (Get (Stack.Stack, Index)));
            end if;
            Operator (Get (Stack.Stack, Index));
         end if;
      end loop;
   end Operate;

   -------------
   -- Operate --
   -------------

   procedure Operate (Stack          : in Evaluation_Stack;
                      Operator       : in Stack_Transformer;
                      Cross_Boundary : in Boolean)
   is
   begin
      for Index in reverse 1 .. Stack.Top loop
         if Get (Stack.Stack, Index) = Boundary_Object then
            exit when not Cross_Boundary;
         else
            if Debug_Stacks then
               Ada.Text_IO.Put_Line ("operating on" & Index'Img & ": " &
                                     SK.Images.Image
                                     (Get (Stack.Stack, Index)));
               Ada.Text_IO.Put_Line ("operating on" & Index'Img & ": " &
                                     SK.Images.Low_Level_Image
                                     (Get (Stack.Stack, Index)));
            end if;
            Set (Stack.Stack, Index, Operator (Get (Stack.Stack, Index)));
            if Debug_Stacks then
               Ada.Text_IO.Put_Line ("New value is " &
                                     Images.Hex_Image
                                     (Get (Stack.Stack, Index)));
            end if;
         end if;
      end loop;
   end Operate;

   ---------
   -- Pop --
   ---------

   procedure Pop (From_Stack : in     Evaluation_Stack;
                  Item       :    out Object)
   is
   begin
      Item := Get (From_Stack.Stack, From_Stack.Top);
      if Item = Boundary_Object then
         Dump_Stack (From_Stack, True);
         raise Stack_Error;
      end if;

      if Debug_Stacks then
         Ada.Text_IO.Put_Line
           ("Pop" & From_Stack.Top'Img & ": " & SK.Images.Image (Item) & "}");
      end if;

      From_Stack.Top := From_Stack.Top - 1;
      Drop (From_Stack.Stack, 1);

   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (To_Stack   : in Evaluation_Stack;
                   Item       : in Object)
   is
   begin
      To_Stack.Top := To_Stack.Top + 1;
      Append (To_Stack.Stack, Item);

      if Item /= Boundary_Object then
         --           Ada.Text_IO.Put_Line ("Push: " &

--                                 SK.Images.Low_Level_Image (Item));
         if Debug_Stacks then
            Ada.Text_IO.Put_Line ("Push" &
                                  To_Stack.Top'Img &
                                  " {" &
                                  SK.Images.Image (Item));
         end if;
      end if;

      if To_Stack.Top > Max_Stack_Depth then
         Max_Stack_Depth := To_Stack.Top;
      end if;

   end Push;

   ---------
   -- Put --
   ---------

   procedure Put (To_Stack   : Evaluation_Stack;
                  Index      : Positive;
                  Value      : Object)
   is
   begin
      if Debug_Stacks then
         Ada.Text_IO.Put_Line ("Stack" & Index'Img & " := " &
                               SK.Images.Image (Value));
      end if;
      Set (To_Stack.Stack, To_Stack.Top - Index + 1, Value);
   end Put;

   ------------------
   -- Set_Boundary --
   ------------------

   procedure Set_Boundary (Stack : in out Evaluation_Stack) is
   begin
      Push (Stack, Boundary_Object);
      Stack.Last_Boundary := Stack.Top;
      Stack.Boundaries := Stack.Boundaries + 1;
   end Set_Boundary;


   ----------
   -- Size --
   ----------

   function Size (Stack : Evaluation_Stack) return Natural is
   begin
      if Stack.Last_Boundary /= 0 then
         return Stack.Top - Stack.Last_Boundary;
      else
         return Stack.Top;
      end if;
   end Size;

   ---------
   -- Top --
   ---------

   function Top (Of_Stack : Evaluation_Stack) return Object is
   begin
      if Of_Stack.Top = 0 then
         raise Stack_Error;
      else
         return Get (Of_Stack.Stack, Of_Stack.Top);
      end if;
   end Top;

   ---------
   -- Top --
   ---------

   function Top (Of_Stack : Evaluation_Stack;
                 Count    : Natural)
                return Array_Of_Objects
   is
      Result : Array_Of_Objects (1 .. Count);
   begin
      pragma Assert (Size (Of_Stack) >= Count);
      for I in Result'Range loop
         Result (I) := Get (Of_Stack, I);
      end loop;
      return Result;
   end Top;

end SK.Stacks;

