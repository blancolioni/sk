with Ada.Strings.Unbounded;
with Ada.Text_IO;

with SK.Debug;
with SK.Debug_Class;
with SK.Environments;
with SK.Images;
with SK.Stack;

package body SK.Functions is

   function Debug_Functions return Boolean;

   Max_Functions : constant := 1000;
   Num_Functions : Function_Id  := 0;

   type Function_Entry is
      record
         Name      : Ada.Strings.Unbounded.Unbounded_String;
         Arg_Count : Natural;
         Eval      : Evaluator;
      end record;

   type Array_Of_Functions is
     array (Function_Id range 1 .. Max_Functions) of Function_Entry;

   Function_List : Array_Of_Functions;

   function "+" (Item : String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count (Id : Function_Id) return Natural is
   begin
      return Function_List (Id).Arg_Count;
   end Argument_Count;

   -------------------
   -- Bind_Function --
   -------------------

   procedure Bind_Function
     (Name         : String;
      Args         : Natural;
      Handler      : Evaluator)
   is
      Fun : Object;
   begin
      Num_Functions := Num_Functions + 1;
      Fun := To_Object (Num_Functions);
      Function_List (Num_Functions) :=
        (+Name, Args, Handler);
      SK.Environments.Define (SK.Environments.Top_Level_Environment,
                              Name, Fun);
      if Debug_Functions then
         Ada.Text_IO.Put_Line ("Adding:" & Num_Functions'Img & ' ' & Name);
      end if;
   end Bind_Function;

   -----------
   -- Check --
   -----------

   procedure Check (Item          : in Object;
                    Function_Name : in String;
                    Checker       : in Check_Function;
                    Message       : in String)
   is
   begin
      if not Checker (Item) then
         Ada.Text_IO.Put_Line ("In function '" & Function_Name & "':");
         Ada.Text_IO.Put_Line ("    " & Hex_Image (Item) &
                               ": " & Message);
         raise Type_Error;
      end if;
   end Check;

   ---------------------
   -- Debug_Functions --
   ---------------------

   function Debug_Functions return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug.Functions);
   end Debug_Functions;

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

         declare
            Local_Args : Array_Of_Objects (1 .. F.Arg_Count);
            Top        : Object;
         begin
            SK.Stack.Pop (Cells, Local_Args);

            if F.Arg_Count > 0 then
               Top := Local_Args (Local_Args'Last);
            end if;

            if Debug_Functions then
               Ada.Text_IO.Put_Line ("  top = "
                                     & SK.Images.Image (Cells, Top));
            end if;

            for I in Local_Args'Range loop
               Local_Args (I) := SK.Cells.Cdr (Cells, Local_Args (I));
            end loop;

            Result := F.Eval (Cells, Local_Args);
            if F.Arg_Count > 0 then
               SK.Cells.Set_Car (Cells, Top, I);
               SK.Cells.Set_Cdr (Cells, Top, Result);
               if Debug_Functions then
                  Ada.Text_IO.Put_Line ("  top = "
                                        & SK.Images.Image (Cells, Top));
               end if;
            end if;
            Changed := True;

            if Debug_Functions then
               Ada.Text_IO.Put_Line ("Result: " &
                                     Hex_Image (Result));
            end if;
         end;

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

end SK.Functions;
