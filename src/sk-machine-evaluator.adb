with Ada.Text_IO;

with SK.Debug;

with SK.Objects.Bindings;

package body SK.Machine.Evaluator is

   function Debug_Eval return Boolean;

   procedure Eval_App
     (Machine : in out SK_Machine_Record'Class);

   procedure Make_Tree
     (Machine : in out SK_Machine_Record'Class;
      Tail    : in     SK.Objects.Object);

   ----------------
   -- Debug_Eval --
   ----------------

   function Debug_Eval return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug.Eval);
   end Debug_Eval;

   ----------
   -- Eval --
   ----------

   procedure Eval
     (Machine : in out SK_Machine_Record'Class)
   is
      use SK.Objects;
      Item : constant SK.Objects.Object := Machine.Pop;
   begin
      if Is_Symbol (Item) then
         raise Program_Error with "unexpected symbol object: "
           & Machine.Show (Item);
      elsif not Is_Application (Item) and then not Is_Function (Item) then
         Machine.Push (Item);
      elsif Is_Application (Item) and then Machine.Left (Item) = I then
         Machine.Push (Machine.Right (Item));
         Evaluate (Machine);
      else
         Machine.Push_Control (Item);
         Eval_App (Machine);
      end if;
   end Eval;

   --------------
   -- Eval_App --
   --------------

   procedure Eval_App
     (Machine : in out SK_Machine_Record'Class)
   is
      use SK.Objects;
      It      : Object;
      Changed : Boolean := True;
   begin
      if Debug_Eval then
         Ada.Text_IO.Put_Line ("Eval_App");
      end if;

      while Changed loop

         Changed := False;

         It := Machine.Pop_Control;

         if Debug_Eval then
            Ada.Text_IO.Put_Line
              ("it: " & Machine.Show (It)
               & "; control: " & Machine.Show (Machine.Control));
         end if;

         while Is_Application (It) loop
            if Debug_Eval then
               Ada.Text_IO.Put_Line
                 ("Push: " & Hex_Image (It) &
                    " [" & Machine.Show (Machine.Right (It)) & "]");
            end if;

            Machine.Push_Control (It);
            It := Machine.Left (Machine.Top_Control);

         end loop;

         if Debug_Eval then
            Ada.Text_IO.Put_Line ("stop: " &
                                    "[" & Hex_Image (It) & "] " &
                                    Machine.Show (It));
         end if;

         if It = I then
            if Machine.Control_Size_At_Least (1) then
               It := Machine.Right (Machine.Pop_Control);
               Machine.Push_Control (It);
               Changed := True;
            end if;
         elsif It = K then
            if Machine.Control_Size_At_Least (2) then
               declare
                  X : constant Object := Machine.Pop_Control;
                  Y : constant Object := Machine.Pop_Control;
               begin
                  Machine.Set_Left (Y, I);
                  Machine.Set_Right (Y, Machine.Right (X));
                  Machine.Push_Control (Machine.Right (X));
                  if Debug_Eval then
                     Ada.Text_IO.Put_Line
                       ("K: " & Machine.Show (Machine.Top_Control));
                  end if;
                  Changed := True;
               end;
            end if;
         elsif It = S then
            if Machine.Control_Size_At_Least (3) then
               Machine.Push (Machine.Right (Machine.Top_Control (1)));
               Machine.Push (Machine.Right (Machine.Top_Control (3)));
               Machine.Apply;
               Machine.Push (Machine.Right (Machine.Top_Control (2)));
               Machine.Push (Machine.Right (Machine.Top_Control (3)));
               Machine.Apply;
               Machine.Apply;
               if Debug_Eval then
                  Ada.Text_IO.Put_Line ("S: " & Machine.Show (Machine.Top));
               end if;
               Machine.Drop_Control (2);
               Machine.Set_Left (Machine.Top_Control, I);
               Machine.Set_Right (Machine.Top_Control, Machine.Top);
               Machine.Drop_Control;
               Machine.Push_Control (Machine.Pop);
               Changed := True;
            end if;
         elsif It = B then
            if Machine.Control_Size_At_Least (3) then
               Machine.Push (Machine.Right (Machine.Top_Control (1)));
               Machine.Push (Machine.Right (Machine.Top_Control (2)));
               Machine.Push (Machine.Right (Machine.Top_Control (3)));
               Machine.Apply;
               Machine.Apply;
               if Debug_Eval then
                  Ada.Text_IO.Put_Line ("B: " & Machine.Show (Machine.Top));
               end if;
               Machine.Drop_Control (2);
               Machine.Set_Left (Machine.Top_Control, I);
               Machine.Set_Right (Machine.Top_Control, Machine.Top);
               Machine.Drop_Control;
               Machine.Push_Control (Machine.Pop);
               Changed := True;
            end if;
         elsif It = C then
            if Machine.Control_Size_At_Least (3) then
               Machine.Push (Machine.Right (Machine.Top_Control (1)));
               Machine.Push (Machine.Right (Machine.Top_Control (3)));
               Machine.Apply;
               Machine.Push (Machine.Right (Machine.Top_Control (2)));
               Machine.Apply;
               if Debug_Eval then
                  Ada.Text_IO.Put_Line ("C: " & Machine.Show (Machine.Top));
               end if;
               Machine.Drop_Control (2);
               Machine.Set_Left (Machine.Top_Control, I);
               Machine.Set_Right (Machine.Top_Control, Machine.Top);
               Machine.Drop_Control;
               Machine.Push_Control (Machine.Pop);
               Changed := True;
            end if;
         elsif Is_Selection (It) then
            if Machine.Control_Size_At_Least (Selection_Count (It) + 1) then
               Machine.Push_Secondary (Machine.Pop_Control);
               for I in 1 .. Selection_Count (It) loop
                  Machine.Push (Machine.Pop_Control);
               end loop;
               Machine.Push (Machine.Apply (Primitive, It));
               Machine.Push_Control
                 (Machine.Right (Machine.Top_Secondary));
               Machine.Pop_Secondary;
               Changed := True;
            end if;
         elsif Is_Function (It) then
            declare
               use SK.Objects.Bindings;
               Fn : constant Bound_Function_Interface'Class :=
                      Get_Binding (To_Function (It));
            begin
               if Fn.Argument_Count = 0 then
                  Machine.Arg_Count := 0;
                  Machine.Push (Fn.Evaluate (Machine));
                  Machine.Set_Left (Machine.Top_Control, I);
                  Machine.Set_Right (Machine.Top_Control, Machine.Top);
                  Machine.Drop_Control;
                  Machine.Push_Control (Machine.Pop);
                  Changed := True;
               elsif Machine.Control_Size_At_Least (Fn.Argument_Count) then
                  Machine.Push_Secondary (Machine.Pop_Control);
                  for I in 1 .. Fn.Argument_Count - 1 loop
                     if Debug_Eval then
                        Ada.Text_IO.Put_Line
                          ("arg: "
                           & Machine.Show
                             (Machine.Right (Machine.Top_Control)));
                     end if;
                     Machine.Push (Machine.Pop_Control);
                  end loop;
                  Machine.Push (Primitive);
                  Machine.Push (It);
                  Machine.Push (0);
                  Machine.Apply;
                  Machine.Apply;
                  Machine.Push_Control
                    (Machine.Right (Machine.Top_Secondary));
                  Machine.Pop_Secondary;
                  Changed := True;
               end if;
            end;
         elsif Is_Symbol (It) then
            raise Constraint_Error with "unexpected symbol: " &
              Machine.Show (It);
         end if;

         if not Changed then

            Make_Tree (Machine, It);
            It := Machine.Pop;
            if Debug_Eval then
               Ada.Text_IO.Put_Line
                 ("result: " & Machine.Show (It));
            end if;

            if Debug_Eval then
               Ada.Text_IO.Put_Line
                 ("no change: stack = "
                  & Machine.Show (Machine.Stack));
            end if;

            if Machine.Stack /= Nil
              and then Is_Application (Machine.Top)
              and then Machine.Left (Machine.Top) = Primitive
            then
               declare
                  Prim : constant Object := Machine.Right (Machine.Top);
               begin
                  pragma Assert (Is_Selection (Prim)
                                 or else (Is_Application (Prim)
                                   and then Is_Function
                                     (Machine.Left (Prim))
                                      and then Is_Integer
                                        (Machine.Right (Prim))));

                  if Is_Selection (Prim) then
                     declare
                        Count : constant Positive :=
                                  Selection_Count (Prim);
                        Index : constant Positive :=
                                  To_Integer (It);
                        Current : Natural := Count;
                     begin
                        Machine.Drop;
                        for I in 1 .. Count loop
                           if Current = Index then
                              Machine.Push_Control
                                (Machine.Right (Machine.Top));
                           end if;
                           Machine.Drop;
                           Current := Current - 1;
                        end loop;
                        Changed := True;
                     end;
                  else
                     declare
                        use SK.Objects.Bindings;
                        Fn : constant Function_Id :=
                               To_Function (Machine.Left (Prim));
                        Binding : constant Bound_Function_Interface'Class :=
                                    Get_Binding (Fn);
                        Args_Seen : constant Natural :=
                                      To_Integer (Machine.Right (Prim));
                     begin
                        if Args_Seen = Binding.Argument_Count - 1 then
                           Machine.Drop;
                           if Debug_Eval then
                              Ada.Text_IO.Put_Line
                                ("Evaluating: "
                                 & Machine.Show (To_Object (Fn))
                                 & " with "
                                 & Machine.Show (It) & " "
                                 & Machine.Show (Machine.Stack));
                              Ada.Text_IO.Put_Line
                                ("Control: " & Machine.Show (Machine.Control));
                           end if;

                           for I in reverse
                             1 .. Binding.Argument_Count - 1
                           loop
                              Machine.Args (I) := Machine.Pop;
                              if Debug_Eval then
                                 Ada.Text_IO.Put_Line
                                   ("arg" & I'Img & " = " &
                                      Machine.Show (Machine.Args (I)));
                              end if;
                           end loop;
                           Machine.Args (Binding.Argument_Count) := It;
                           Machine.Arg_Count := Binding.Argument_Count;

                           Machine.Push_Control
                             (Binding.Evaluate (Machine));

                           Machine.Set_Left (Machine.Top, I);
                           Machine.Set_Right
                             (Machine.Top, Machine.Top_Control);
                           Machine.Drop;

                        else
                           if Debug_Eval then
                              Ada.Text_IO.Put_Line
                                ("Partial primitive: "
                                 & Machine.Show (To_Object (Fn))
                                 & "; control = "
                                 & Machine.Show (Machine.Control));
                           end if;

                           Machine.Drop;
                           Machine.Push_Secondary (It);
                           for I in 1 .. Args_Seen loop
                              Machine.Push_Secondary (Machine.Pop);
                           end loop;
                           Machine.Push_Control
                             (Machine.Right (Machine.Top));
                           for I in 1 .. Args_Seen + 1 loop
                              Machine.Push (Machine.Top_Secondary);
                              Machine.Pop_Secondary;
                           end loop;

                           if Debug_Eval then
                              Ada.Text_IO.Put_Line
                                ("Saving partial: "
                                 & Machine.Show (To_Object (Fn))
                                 & " with "
                                 & Machine.Show (Machine.Stack));
                              Ada.Text_IO.Put_Line
                                ("Next argument: "
                                 & Machine.Show (Machine.Top_Control));
                           end if;

                           Machine.Push (Primitive);
                           Machine.Push (To_Object (Fn));
                           Machine.Push (Args_Seen + 1);
                           Machine.Apply;
                           Machine.Apply;
                        end if;
                        Changed := True;
                     end;

                  end if;
               end;
            else

               Machine.Push (It);

            end if;
         end if;
      end loop;

      if Debug_Eval then
         Ada.Text_IO.Put_Line ("Done");
      end if;

   end Eval_App;

   ---------------
   -- Make_Tree --
   ---------------

   procedure Make_Tree
     (Machine : in out SK_Machine_Record'Class;
      Tail    : in     SK.Objects.Object)
   is
      use SK.Objects;
      Count : Natural := 0;
   begin
      if Debug_Eval then
         Ada.Text_IO.Put_Line ("building result: start = "
                               & Machine.Show (Tail));
      end if;

      Machine.Push (Tail);
      while Machine.Control /= Nil loop
         Machine.Push (Machine.Right (Machine.Pop_Control));
         Count := Count + 1;
      end loop;

      for I in 1 .. Count loop
         Machine.Apply;
      end loop;

   end Make_Tree;

end SK.Machine.Evaluator;
