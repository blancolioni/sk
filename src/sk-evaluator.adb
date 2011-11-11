with Ada.Text_IO;

with SK.Combinators;
with SK.Debug;
with SK.Functions;
with SK.Images;
with SK.Stack;

package body SK.Evaluator is

   function Debug_Eval return Boolean;

   function Eval_App (Cells  : SK.Cells.Managed_Cells;
                      Item   : Object)
                     return Object;

   procedure Make_Tree (Cells  : in     SK.Cells.Managed_Cells;
                        Tail   : in     Object;
                        Result :    out Object);

   ----------------
   -- Debug_Eval --
   ----------------

   function Debug_Eval return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug.Eval);
   end Debug_Eval;

   --------------
   -- Eval_App --
   --------------

   function Eval_App (Cells  : SK.Cells.Managed_Cells;
                      Item   : Object)
                     return Object
   is
      It          : Object := Item;
      Changed     : Boolean := True;
   begin
      pragma Assert (Is_Application (Item) or Is_Function (Item));

--        if Debug_Eval then
--           Ada.Text_IO.Put_Line ("Eval: " & SK.Images.Image (Cells, Item));
--        end if;

      while Changed loop

         Changed := False;

         while Is_Application (It) loop
            if Debug_Eval then
               Ada.Text_IO.Put_Line
                 ("Push: " & Hex_Image (It) &
                  " [" & Hex_Image (SK.Cells.Car (Cells, It))
                  & " " & Hex_Image (SK.Cells.Cdr (Cells, It)) & "]");
            end if;

            SK.Stack.Push (Cells, It);
            It := SK.Cells.Car (Cells, SK.Stack.Top (Cells));

         end loop;

         if Debug_Eval then
            Ada.Text_IO.Put_Line ("stop: " &
                                  "[" & Hex_Image (It) & "] " &
                                  SK.Images.Image (Cells, It));
         end if;

         if Is_Combinator (It) then
            It := SK.Combinators.Evaluate (Cells, Get_Combinator (It));
            Changed := True;
         elsif Is_Function (It) then
            declare
               Function_Result : Object;
            begin
               if Debug_Eval then
                  Ada.Text_IO.Put_Line ("evaluating function: " &
                                        Hex_Image (It));
               end if;

               SK.Functions.Evaluate (Get_Function_Id (It),
                                      Cells,
                                      Function_Result,
                                      Changed);
               if Changed then
                  It := Function_Result;
                  if Debug_Eval then
                     Ada.Text_IO.Put_Line ("Result = " &
                                           Hex_Image
                                           (Function_Result));
                  end if;
               end if;
            exception
               when Evaluation_Error =>
                  Ada.Text_IO.Put_Line ("   " & Hex_Image (It));
                  raise;

            end;
         elsif Is_Symbol (It) then
            raise Constraint_Error with "unexpected symbol: " &
              SK.Images.Image (Cells, It);
         end if;

      end loop;

      if Debug_Eval then
         Ada.Text_IO.Put_Line ("Done");
      end if;

      declare
         Result : Object;
      begin
         Make_Tree (Cells, It, Result);
         return Result;
      end;

   end Eval_App;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate (Cells     : in     SK.Cells.Managed_Cells;
                       Item      : in out Object)
   is
   begin
      if Is_Symbol (Item) then
         raise Program_Error with "unexpected symbol object: " &
         SK.Images.Image (Cells, Item);
      elsif not Is_Application (Item) and not Is_Function (Item) then
         null;
      elsif Is_Application (Item) and then SK.Cells.Car (Cells, Item) = I then
         Item := Evaluate (Cells, SK.Cells.Cdr (Cells, Item));
      else
         SK.Stack.Set_Boundary (Cells);
         Item := Eval_App (Cells, Item);
         SK.Stack.Clear_Boundary (Cells);
      end if;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Cells     : in SK.Cells.Managed_Cells;
                      Item      : in Object)
                      return Object
   is
      E : Object := Item;
   begin
      Evaluate (Cells, E);
      return E;
   end Evaluate;

   ---------------
   -- Make_Tree --
   ---------------

   procedure Make_Tree (Cells  : in     SK.Cells.Managed_Cells;
                        Tail   : in     Object;
                        Result :    out Object)
   is

      Nodes : constant Array_Of_Objects := SK.Stack.Pop_All (Cells);
      T     : Object;
   begin
      Result := Tail;
      for I in reverse Nodes'Range loop
         T := Nodes (I);
         SK.Cells.Set_Car (Cells, T, Result);
         Result := T;
      end loop;
   end Make_Tree;


end SK.Evaluator;
