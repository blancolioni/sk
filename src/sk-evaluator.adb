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
      It             : Object := Item;
      Changed        : Boolean := True;
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
            declare
               New_It : constant Object :=
                          SK.Combinators.Evaluate
                            (Cells, Get_Combinator (It));
            begin
               if New_It /= Fail_Object then
                  It := New_It;
                  Changed := True;
               end if;
            end;
         elsif Is_Pick (It) then
            if SK.Stack.Minimum_Count (Cells, Num_Picks (It) + 1) then
               declare
                  New_It : constant Object :=
                             SK.Stack.Top (Cells);
               begin
                  SK.Stack.Push (Cells, It);
                  SK.Stack.Push (Cells, Last_Argument);
                  SK.Stack.Push (Cells, New_It);
                  SK.Stack.Push (Cells, Next_Argument);
                  It := SK.Cells.Cdr (Cells, New_It);
               end;
               Changed := True;
            end if;
         elsif Is_Function (It) then
            declare
               Id : constant Function_Id :=
                      Get_Function_Id (It);
               Arg_Count : constant Natural :=
                             SK.Functions.Argument_Count (Id);
               Args      : Array_Of_Objects (1 .. Arg_Count);
            begin
               if not SK.Stack.Minimum_Count (Cells, Arg_Count) then
                  Changed := False;
               else
                  SK.Stack.Top (Cells, Args);
                  SK.Stack.Push (Cells, It);
                  SK.Stack.Push (Cells, Last_Argument);

                  for I in 1 .. Arg_Count loop
                     SK.Stack.Push (Cells, Args (I));
                     SK.Stack.Push (Cells, Next_Argument);
                     SK.Stack.Push (Cells, SK.Cells.Cdr (Cells, Args (I)));

                     Changed := True;
                  end loop;

                  if Changed then
                     SK.Stack.Pop (Cells, It);
                  end if;
               end if;
            end;

         elsif Is_Symbol (It) then
            raise Constraint_Error with "unexpected symbol: " &
              SK.Images.Image (Cells, It);
         end if;

         if not Changed then
            if SK.Stack.Top (Cells) = Next_Argument then
               declare
                  T : Object;
               begin
                  SK.Stack.Pop (Cells, T);
                  SK.Stack.Pop (Cells, T);
                  SK.Cells.Set_Cdr (Cells, T, It);
               end;

               SK.Stack.Pop (Cells, It);
               Changed := True;

               if It = Last_Argument then
                  SK.Stack.Pop (Cells, It);
                  pragma Assert (Is_Function (It) or else Is_Pick (It));

                  if Is_Function (It) then
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

                  else
                     declare
                        Pick_Object : constant Object := SK.Stack.Pop (Cells);
                        Pick        : constant Natural :=
                                        Get_Integer
                                          (SK.Cells.Cdr (Cells, Pick_Object));
                        Args        : Array_Of_Objects
                          (1 .. Num_Picks (It));
                     begin
                        SK.Stack.Pop (Cells, Args);
                        if Debug_Eval then
                           for I in Args'Range loop
                              Ada.Text_IO.Put_Line
                                (" choice" & Integer'Image (I - 1) & ": "
                                 & SK.Images.Image (Cells, Args (I)));
                           end loop;
                           Ada.Text_IO.Put_Line ("choosing:" &
                                                 Pick'Img);
                        end if;
                        It := SK.Cells.Cdr (Cells, Args (Pick + 1));
                        Changed := True;
                     end;
                  end if;
               end if;
            end if;
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
