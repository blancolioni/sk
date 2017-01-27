with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

with SK.Compiler;
with SK.Objects.Symbols;

with SK.Machine.Evaluator;

with SK.Primitives;

package body SK.Machine is

   Trace_Stack : constant Boolean := False;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (SK.Objects.External_Object_Interface'Class,
        External_Object_Access);

   --------------
   -- After_GC --
   --------------

   overriding procedure After_GC (Machine : in out SK_Machine_Record) is
   begin
      for I in 1 .. Machine.External_Objects.Last_Index loop
         declare
            Item : External_Object_Record renames
                     Machine.External_Objects (I);
         begin
            if Item.Marked then
               Item.Marked := False;
            elsif not Item.Free then
               Item.External_Object.Finalize (Machine);
               Free (Item.External_Object);
               Item.Free := True;
            end if;
         end;
      end loop;
   end After_GC;

   -----------
   -- Apply --
   -----------

   overriding procedure Apply
     (Machine : in out SK_Machine_Record)
   is
      Cdr : constant SK.Objects.Object := Machine.Pop;
      Car : constant SK.Objects.Object := Machine.Pop;
      T   : constant SK.Objects.Object :=
              Machine.Apply (Car, Cdr);
   begin
      if Trace_Stack then
         Ada.Text_IO.Put_Line
           ("machine: cons --> " & Machine.Show (T));
      end if;
      Machine.Push (T);
   end Apply;

   ---------------
   -- Before_GC --
   ---------------

   overriding procedure Before_GC
     (Machine : in out SK_Machine_Record)
   is
      procedure Mark (Value : in out SK.Objects.Object);

      ----------
      -- Mark --
      ----------

      procedure Mark (Value : in out SK.Objects.Object) is
      begin
         SK.Memory.Mark (Machine.Core, Value);
      end Mark;
   begin

      for I in Machine.R'Range loop
         Mark (Machine.R (I));
      end loop;

      for I in Machine.Args'Range loop
         Mark (Machine.Args (I));
      end loop;

      Mark (Machine.Stack);
      Mark (Machine.Secondary_Stack);
      Mark (Machine.Control);
      Mark (Machine.Argument_Stack);

      for I in 1 .. Machine.External_Objects.Last_Index loop
         declare
            Item : External_Object_Record renames
                     Machine.External_Objects (I);
         begin
            Item.Marked := False;
         end;
      end loop;

      declare
         procedure Set_Mark (Item : in out SK.Objects.Object);

         --------------
         -- Set_Mark --
         --------------

         procedure Set_Mark (Item : in out SK.Objects.Object) is
         begin
            Machine.Mark (Item);
         end Set_Mark;

      begin
         Machine.Environment.Update (Set_Mark'Access);
      end;

   end Before_GC;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Machine : in out SK_Machine_Record'Class)
   is
      E : SK.Objects.Object := Machine.Pop;
   begin
      E := SK.Compiler.Compile (Machine, E);
      Machine.Push (E);
   end Compile;

   ---------------------------
   -- Control_Size_At_Least --
   ---------------------------

   function Control_Size_At_Least
     (Machine : SK_Machine_Record'Class;
      Minimum : Natural)
      return Boolean
   is
      use SK.Objects;
      X : Object := Machine.Control;
   begin
      for I in 1 .. Minimum loop
         if X = Nil then
            return False;
         end if;
         X := Machine.Right (X);
      end loop;
      return True;
   end Control_Size_At_Least;

   ------------
   -- Create --
   ------------

   function Create
     (Core_Size : Positive)
      return SK_Machine
   is
      use SK.Objects;
      Machine : constant SK_Machine := new SK_Machine_Record;
   begin
      Machine.Core_Size := Core_Size;
      Machine.Core.Create
        (SK.Objects.Cell_Address (Core_Size),
         SK.Memory.GC_Callback (Machine));

      Machine.Stack := Nil;
      Machine.Secondary_Stack := Nil;
      Machine.Control := Nil;
      Machine.Argument_Stack := Nil;

      SK.Primitives.Load_Primitives (Machine.all);

      return Machine;

   end Create;

   -------------------------------
   -- Create_External_Reference --
   -------------------------------

   overriding function Create_External_Reference
     (Machine : in out SK_Machine_Record;
      External : SK.Objects.External_Object_Interface'Class)
      return SK.Objects.Object
   is
      use SK.Objects;
      Address : External_Object_Id := 0;
      New_Item : constant External_Object_Access :=
                   new SK.Objects.External_Object_Interface'Class'
                     (External);
      New_Entry : constant External_Object_Record :=
                    (External_Object => New_Item,
                     Free            => False,
                     Marked          => False);
   begin
      for I in 1 .. Machine.External_Objects.Last_Index loop
         if Machine.External_Objects (I).Free then
            Address := I;
            exit;
         end if;
      end loop;

      if Address = 0 then
         Machine.External_Objects.Append (New_Entry);
         Address := Machine.External_Objects.Last_Index;
      else
         Machine.External_Objects (Address) := New_Entry;
      end if;

      return SK.Objects.To_Object (Address);
   end Create_External_Reference;

   ----------------------
   -- Define_Top_Level --
   ----------------------

   overriding procedure Define_Symbol
     (Machine : in out SK_Machine_Record;
      Name    : SK.Objects.Symbol_Id;
      Value   : SK.Objects.Object)
   is
   begin
      if Machine.Environment.Contains (Name) then
         Machine.Environment.Replace (Name, Value);
      else
         Machine.Environment.Insert (Name, Value);
      end if;
   end Define_Symbol;

   ------------------
   -- Drop_Control --
   ------------------

   procedure Drop_Control
     (Machine : in out SK_Machine_Record'Class;
      Count   : Positive := 1)
   is
   begin
      for I in 1 .. Count loop
         Machine.Control := Machine.Right (Machine.Control);
      end loop;
   end Drop_Control;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Machine     : in out SK_Machine_Record;
      Expression  : SK.Objects.Object)
      return SK.Objects.Object
   is
   begin
      Machine.Push (Expression);
      Machine.Evaluate;

      declare
         use SK.Objects;
         Result : constant Object := Machine.Pop;
      begin
         if Machine.Stack /= Nil then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "warning: machine state not clean after evaluation");
            Machine.Report_State;
         end if;
         return Result;
      end;

   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate (Machine : in out SK_Machine_Record'Class) is
      use Ada.Calendar;
      Top : constant Boolean := not Machine.Evaluating;
   begin
      if Top then
         Machine.Evaluating := True;
         Machine.Start_Eval := Ada.Calendar.Clock;
      end if;

      SK.Machine.Evaluator.Eval (Machine);

      if Top then
         Machine.Eval_Time := Machine.Eval_Time + (Clock - Machine.Start_Eval);
         Machine.Evaluating := False;
      end if;

   end Evaluate;

   -------------------------
   -- Get_External_Object --
   -------------------------

   overriding function Get_External_Object
     (Machine : SK_Machine_Record;
      Item    : SK.Objects.Object)
      return access SK.Objects.External_Object_Interface'Class
   is
      Address : constant Real_External_Address :=
                  SK.Objects.To_External_Object_Id (Item);
   begin
      return Machine.External_Objects (Address).External_Object;
   end Get_External_Object;

   ----------------
   -- Get_Symbol --
   ----------------

   overriding procedure Get_Symbol
     (Machine : SK_Machine_Record;
      Name    : SK.Objects.Symbol_Id;
      Value   : out SK.Objects.Object;
      Found   : out Boolean)
   is
   begin
      if Machine.Environment.Contains (Name) then
         Value := Machine.Environment.Element (Name);
         Found := True;
      else
         Found := False;
      end if;
   end Get_Symbol;

   -------------
   -- Is_Free --
   -------------

   function Is_Free (Machine : SK_Machine_Record'Class;
                     Address : SK.Objects.Cell_Address)
                     return Boolean
   is
   begin
      return not SK.Memory.Valid (Machine.Core, Address);
   end Is_Free;

   ----------
   -- Link --
   ----------

   procedure Link
     (Machine : in out SK_Machine_Record'Class)
   is
      use SK.Objects;
      function Do_Link (E : Object) return Object;

      -------------
      -- Do_Link --
      -------------

      function Do_Link (E : Object) return Object is
      begin
         if Is_Symbol (E) then
            declare
               Name  : constant Symbol_Id := To_Symbol (E);
               Defn  : Object;
               Found : Boolean;
            begin
               Machine.Get_Symbol (Name, Defn, Found);
               if not Found then
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "link error: "
                     & Symbols.Get_Name (Name) & " not defined");
                  return E;
               else
                  if not Machine.Linked.Contains (Name) then
                     Defn := Do_Link (Defn);
                     Machine.Environment.Replace (Name, Defn);
                     Machine.Linked.Insert (Name, True);
                  end if;
                  return Defn;
               end if;
            end;
         elsif Is_Application (E) then
            Machine.Push_Secondary (E);
            Machine.Push (Do_Link (Machine.Left (Machine.Top_Secondary)));
            Machine.Push (Do_Link (Machine.Right (Machine.Top_Secondary)));
            Machine.Pop_Secondary;
            Machine.Apply;
            return Machine.Pop;
         else
            return E;
         end if;
      end Do_Link;

   begin
      Machine.Push (Do_Link (Machine.Pop));
   end Link;

   ----------
   -- Mark --
   ----------

   overriding procedure Mark
     (Machine : in out SK_Machine_Record;
      Start   : in out SK.Objects.Object)
   is
   begin
      SK.Memory.Mark (Machine.Core, Start);
   end Mark;

   --------------------------
   -- Mark_External_Object --
   --------------------------

   overriding procedure Mark_External_Object
     (Machine : in out SK_Machine_Record;
      External : SK.Objects.External_Object_Id;
      Mark     : not null access
        procedure (X : in out SK.Objects.Object))
   is
   begin
      Machine.External_Objects (External).External_Object.Mark
        (Machine, Mark);
      Machine.External_Objects (External).Marked := True;
   end Mark_External_Object;

   ---------
   -- Pop --
   ---------

   overriding function Pop
     (Machine : in out SK_Machine_Record)
      return SK.Objects.Object
   is
      use SK.Objects;
      Result : constant SK.Objects.Object :=
                 Machine.Left (Machine.Stack);
   begin
      if Trace_Stack then
         Ada.Text_IO.Put_Line
           ("machine: pop " & Machine.Show (Result)
            & " <-- " & Machine.Show (Machine.Stack));
      end if;
      Machine.Stack := Machine.Right (Machine.Stack);
      return Result;
   end Pop;

   -----------------
   -- Pop_Control --
   -----------------

   function Pop_Control
     (Machine : in out SK_Machine_Record'Class)
      return SK.Objects.Object
   is
   begin
      return X : constant SK.Objects.Object :=
        Machine.Left (Machine.Control)
      do
         if Trace_Stack then
            Ada.Text_IO.Put_Line
              ("machine: pop-control " & Machine.Show (X)
               & " <-- " & Machine.Show (Machine.Control));
         end if;
         Machine.Control := Machine.Right (Machine.Control);
      end return;
   end Pop_Control;

   -------------------
   -- Pop_Secondary --
   -------------------

   overriding procedure Pop_Secondary
     (Machine : in out SK_Machine_Record)
   is
   begin
      Machine.Secondary_Stack := Machine.Right (Machine.Secondary_Stack);
   end Pop_Secondary;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (Machine : in out SK_Machine_Record;
      Value   : SK.Objects.Object)
   is
   begin
      Machine.Stack := Machine.Apply (Value, Machine.Stack);
      if Trace_Stack then
         Ada.Text_IO.Put_Line
           ("machine: push " & Machine.Show (Value)
            & " --> " & Machine.Show (Machine.Stack));
      end if;
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (Machine : in out SK_Machine_Record;
      Symbol_Name : String)
   is
      use SK.Objects, SK.Objects.Symbols;
   begin
      Machine.Push
        (To_Object (Get_Symbol_Id (Symbol_Name)));
   end Push;

   ------------------
   -- Push_Control --
   ------------------

   procedure Push_Control
     (Machine : in out SK_Machine_Record'Class;
      Value   : SK.Objects.Object)
   is
   begin
      if Trace_Stack then
         Ada.Text_IO.Put_Line
           ("Push control: stack = "
            & Machine.Show (Machine.Control));
      end if;

      Machine.Push (Value);
      Machine.Push (Machine.Control);
      Machine.Apply;
      Machine.Control := Machine.Pop;

      if Trace_Stack then
         Ada.Text_IO.Put_Line
           ("Push control: stack = "
            & Machine.Show (Machine.Control));
      end if;

   end Push_Control;

   --------------------
   -- Push_Secondary --
   --------------------

   overriding procedure Push_Secondary
     (Machine : in out SK_Machine_Record;
      Value   : SK.Objects.Object)
   is
   begin
      Machine.Secondary_Stack :=
        Machine.Apply (Value, Machine.Secondary_Stack);
   end Push_Secondary;

   -------------------
   -- Report_Memory --
   -------------------

   procedure Report_Memory
     (Machine : SK_Machine_Record'Class)
   is
   begin
      SK.Memory.Report (Machine.Core);
   end Report_Memory;

   ------------------
   -- Report_State --
   ------------------

   overriding procedure Report_State
     (Machine : in out SK_Machine_Record)
   is
   begin
      Machine.Report_Memory;
      Ada.Text_IO.Put_Line
        (" S: " & Machine.Show (Machine.Stack));
      Ada.Text_IO.Put_Line
        (" D: " & Machine.Show (Machine.Secondary_Stack));
      Ada.Text_IO.Put_Line
        (" A: " & Machine.Show (Machine.Argument_Stack));
      Ada.Text_IO.Put_Line
        (" C: " & Machine.Show (Machine.Control));
      Ada.Text_IO.Put_Line
        ("Eval:"
         & Natural'Image (Natural (Machine.Eval_Time * 1000.0))
         & "ms");
   end Report_State;

   --------------
   -- Set_Left --
   --------------

   overriding procedure Set_Left
     (Machine     : in out SK_Machine_Record;
      Application : in     SK.Objects.Object;
      New_Left    : in SK.Objects.Object)
   is
   begin
      Machine.Core.Set_Car (SK.Objects.Get_Address (Application),
                            New_Left);
   end Set_Left;

   ---------------
   -- Set_Right --
   ---------------

   overriding procedure Set_Right
     (Machine     : in out SK_Machine_Record;
      Application : in     SK.Objects.Object;
      New_Right   : in SK.Objects.Object)
   is
   begin
      Machine.Core.Set_Cdr (SK.Objects.Get_Address (Application),
                            New_Right);
   end Set_Right;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Machine : in out SK_Machine_Record;
      Value   : SK.Objects.Object)
      return String
   is
      use SK.Objects;
   begin
      if Value = Nil then
         return "()";
      elsif Is_Integer (Value) then
         return Ada.Strings.Fixed.Trim
           (Integer'Image (To_Integer (Value)),
            Ada.Strings.Left);
      elsif Is_Symbol (Value) then
         return Symbols.Get_Name (To_Symbol (Value));
      elsif Is_Function (Value) then
         return Show (To_Function (Value));
      elsif Is_External_Object (Value) then
         return Machine.Get_External_Object (Value).Print (Machine);
      elsif Is_Selection (Value) then
         return "select" & Integer'Image (-Selection_Count (Value));
      elsif Is_Application (Value) then
         declare
            Car : constant Object := Machine.Left (Value);
            Cdr : constant Object := Machine.Right (Value);
            Left_Lambda : constant Boolean :=
                            Is_Application (Car)
                              and then Is_Application (Machine.Left (Car))
              and then Machine.Left (Machine.Left (Car)) = Lambda;
         begin
            if Is_Application (Car)
              and then Machine.Left (Car) = Lambda
            then
               return "\" & Machine.Show (Machine.Right (Car))
                 & "." & Machine.Show (Cdr);
            elsif Is_Application (Cdr) then
               if Left_Lambda then
                  return "(" & Machine.Show (Car) & ") ("
                    & Machine.Show (Cdr) & ")";
               else
                  return Machine.Show (Car) & " ("
                    & Machine.Show (Cdr) & ")";
               end if;
            elsif Left_Lambda then
               return "(" & Machine.Show (Car) & ") "
                 & Machine.Show (Cdr);
            else
               return Machine.Show (Car) & " "
                 & Machine.Show (Cdr);
            end if;
         end;
      elsif Is_Symbol (Value) then
         return SK.Objects.Symbols.Get_Name (SK.Objects.To_Symbol (Value));
      elsif Value = S then
         return "S";
      elsif Value = K then
         return "K";
      elsif Value = I then
         return "I";
      elsif Value = C then
         return "C";
      elsif Value = B then
         return "B";
      elsif Value = Lambda then
         return "\";
      elsif Value = Primitive then
         return "[primitive]";
      else
         return "<error: unknown object type ["
           & Hex_Image (Value)
           & "]";
      end if;
   end Show;

   ---------
   -- Top --
   ---------

   overriding function Top
     (Machine : SK_Machine_Record;
      Index   : Positive := 1)
      return SK.Objects.Object
   is

      function Get_Nth
        (From : SK.Objects.Object;
         N    : Positive)
         return SK.Objects.Object
      is (if N = 1
          then Machine.Left (From)
          else Get_Nth (Machine.Right (From), N - 1));

   begin
      return Get_Nth (Machine.Stack, Index);
   end Top;

   -----------------
   -- Top_Control --
   -----------------

   function Top_Control
     (Machine : SK_Machine_Record'Class;
      Index   : Positive := 1)
      return SK.Objects.Object
   is
      It : SK.Objects.Object := Machine.Control;
   begin
      for I in 1 .. Index - 1 loop
         It := Machine.Right (It);
      end loop;
      return Machine.Left (It);
   end Top_Control;

end SK.Machine;
