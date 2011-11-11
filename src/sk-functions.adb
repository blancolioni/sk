with Ada.Strings.Unbounded;
with Ada.Text_IO;

with SK.Evaluator;
with SK.Images;
with SK.Stack;

with SK.Debug;

package body SK.Functions is

   function Debug_Definitions return Boolean;
   function Debug_Functions return Boolean;

   Max_Functions : constant := 1000;
   Num_Functions : Function_Id  := 0;

   type Function_Entry is
      record
         Name         : Ada.Strings.Unbounded.Unbounded_String;
         Arg_Count    : Natural;
         Eval         : Lazy_Evaluator;
         Strict_Eval  : Evaluator;
         Internal     : Boolean;
         Strict       : Boolean;
         Lazy_Result  : Boolean;
      end record;

   type Array_Of_Functions is
     array (Function_Id range 1 .. Max_Functions) of Function_Entry;

   Function_List : Array_Of_Functions;

   function "+" (Item : String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Ground_Argument (Cells : SK.Cells.Managed_Cells;
                              Index    : Positive);

   function Ground_Argument_In_Place (Cells : SK.Cells.Managed_Cells;
                                      Arg   : Object)
                                     return Object;
   --  Evaluates the right child of Arg in place, and returns it.

   function Debug_Pattern_Matching return Boolean;


   ---------------------------
   -- Add_External_Function --
   ---------------------------

   procedure Add_External_Function (Env      : SK.Environments.Environment;
                                    Name     : String;
                                    Args     : Natural;
                                    Handler  : Evaluator;
                                    Strict   : Boolean)
   is
   begin
      Add_Function (Env, Name, Args, null, Handler,
                    Internal => False, Strict => Strict);
   end Add_External_Function;



   ------------------
   -- Add_Function --
   ------------------

   procedure Add_Function (Env         : SK.Environments.Environment;
                           Name           : String;
                           Args           : Natural;
                           Handler        : Lazy_Evaluator;
                           Strict_Handler : Evaluator;
                           Internal       : Boolean;
                           Strict         : Boolean;
                           Lazy_Result    : Boolean   := True)
   is
      Fun : Object;
   begin
      Num_Functions := Num_Functions + 1;
      Fun := To_Object (Num_Functions);
      Function_List (Num_Functions) :=
        (+Name, Args, Handler, Strict_Handler, Internal, Strict, Lazy_Result);
      SK.Environments.Define (Env, Name, Fun);
      if Debug_Functions then
         Ada.Text_IO.Put_Line ("Adding:" & Num_Functions'Img & ' ' & Name);
      end if;
   end Add_Function;

   ---------------------------
   -- Add_Internal_Function --
   ---------------------------

   procedure Add_Internal_Function (Env         : SK.Environments.Environment;
                                    Name        : String;
                                    Args        : Natural;
                                    Handler     : Lazy_Evaluator;
                                    Lazy_Result : Boolean    := True)
   is
   begin
      Add_Function (Env, Name, Args, Handler, null, Internal => True,
                    Strict => False, Lazy_Result => Lazy_Result);
   end Add_Internal_Function;

   -----------
   -- Check --
   -----------

   procedure Check (Cells         : in SK.Cells.Managed_Cells;
                    Item          : in Object;
                    Function_Name : in String;
                    Checker       : in Check_Function;
                    Message       : in String)
   is
   begin
      if not Checker (Item) then
         Ada.Text_IO.Put_Line ("In function '" & Function_Name & "':");
         Ada.Text_IO.Put_Line ("    " & SK.Images.Image (Cells, Item) &
                               ": " & Message);
         raise Type_Error;
      end if;
   end Check;

   -----------------------
   -- Debug_Definitions --
   -----------------------

   function Debug_Definitions return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug.Definitions);
   end Debug_Definitions;

   ---------------------
   -- Debug_Functions --
   ---------------------

   function Debug_Functions return Boolean is
   begin
      return False; --  SK.Debug.Enabled (SK.Debug_Class.Functions);
   end Debug_Functions;

   ----------------------------
   -- Debug_Pattern_Matching --
   ----------------------------

   function Debug_Pattern_Matching return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug.Pattern_Matching);
   end Debug_Pattern_Matching;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate (Id      : in     Function_Id;
                       Cells   : in     SK.Cells.Managed_Cells;
                       Result  :    out Object;
                       Changed :    out Boolean)
   is
      F : Function_Entry renames Function_List (Id);
   begin
      if Debug_Functions then
         Ada.Text_IO.Put_Line ("Evalute: " &
                               Ada.Strings.Unbounded.To_String (F.Name));
      end if;
      if SK.Stack.Minimum_Count (Cells, F.Arg_Count) then

         if F.Arg_Count > 0 and F.Lazy_Result then
            declare
               Tmp : Array_Of_Objects (1 .. F.Arg_Count);
            begin
               SK.Stack.Pop (Cells, Tmp);
               SK.Stack.Push (Cells, Tmp (Tmp'Last));
               for I in reverse Tmp'Range loop
                  SK.Stack.Push (Cells, Tmp (I));
               end loop;
            end;
         end if;

         if not F.Internal then
            declare
               Local_Args : Array_Of_Objects (1 .. F.Arg_Count);
            begin
               for I in Local_Args'Range loop
                  Ground_Argument (Cells, I);
               end loop;
               for I in Local_Args'Range loop
                  Local_Args (I) := SK.Stack.Get (Cells, I);
               end loop;
               Result := F.Strict_Eval (Cells, Local_Args);
            end;
         else

            if F.Strict then
               --  set the stack up nicely
               for I in 1 .. F.Arg_Count loop
                  Ground_Argument (Cells, I);
               end loop;
            end if;

            Result := F.Eval (Cells);

         end if;

         SK.Stack.Drop (Cells, Object (F.Arg_Count));

         if F.Arg_Count > 0 and F.Lazy_Result then
            SK.Cells.Set_Car (Cells, SK.Stack.Top (Cells), I);
            SK.Cells.Set_Cdr (Cells, SK.Stack.Top (Cells), Result);
            SK.Stack.Drop (Cells, 1);
         end if;

         Changed := True;

         if Debug_Functions then
            Ada.Text_IO.Put_Line ("Result: " &
                                  Hex_Image (Result));
         end if;

      else
         Changed := False;
         if Debug_Functions then
            Ada.Text_IO.Put_Line ("no result");
         end if;
      end if;
   end Evaluate;


   -----------------------
   -- Get_Function_Name --
   -----------------------

   function Get_Function_Name (Id : Function_Id) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Function_List (Id).Name);
   end Get_Function_Name;

   ---------------------
   -- Ground_Argument --
   ---------------------

   procedure Ground_Argument (Cells : SK.Cells.Managed_Cells;
                              Index    : Positive)
   is
      Unused : constant Object := Ground_Argument_Index (Cells, Index);
      pragma Unreferenced (Unused);
   begin
      null;
   end Ground_Argument;

   ------------------------------
   -- Ground_Argument_In_Place --
   ------------------------------

   function Ground_Argument_In_Place (Cells : SK.Cells.Managed_Cells;
                                      Arg   : Object)
                                     return Object
   is
      Item : Object := SK.Cells.Cdr (Cells, Arg);
   begin
      if Debug_Functions then
         Ada.Text_IO.Put_Line ("Ground: " &
                               SK.Images.Image (Cells, Item));
         Ada.Text_IO.Put_Line ("Ground: " &
                               Hex_Image (Item));
      end if;

      SK.Evaluator.Evaluate (Cells, Item);

      SK.Cells.Set_Cdr (Cells, Arg, Item);

      if Debug_Functions then
         Ada.Text_IO.Put_Line ("Grounded arg = " &
                               SK.Images.Image (Cells, Item));
      end if;

      return Item;
   end Ground_Argument_In_Place;

   ---------------------------
   -- Ground_Argument_Index --
   ---------------------------

   function Ground_Argument_Index (Cells : SK.Cells.Managed_Cells;
                                   Index : Positive)
                                   return Object
   is
      Value  : constant Object := SK.Stack.Get (Cells, Index);
      Ground : constant Object := Ground_Argument_In_Place (Cells, Value);
   begin
      if Debug_Functions then
         Ada.Text_IO.Put_Line ("Ground_Argument: value = " &
                                 SK.Images.Image (Cells, Value));
         Ada.Text_IO.Put_Line ("Ground_Argument: ground = " &
                               SK.Images.Image (Cells, Ground));
      end if;
      SK.Stack.Put (Cells, Index, Ground);
      return Ground;
   end Ground_Argument_Index;

end SK.Functions;
