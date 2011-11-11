with Ada.Text_IO;

with WL.Lists.Generic_List;

with SK.Debug;
with SK.Debug_Class;
with SK.Environments;
with SK.Images;
with SK.Interfaces;

package body SK.Allocation is

   package List_Of_Objects is
      new WL.Lists.Generic_List (Object);


   function Debug_GC return Boolean;
   function Debug_GC_Details return Boolean;
   function Debug_Images return Boolean;
   function Debug_Memory return Boolean;

   function Show_Hex (Value : Object) return String;

   Max_Stacks : constant := 10;
   type Stack_Count is range 0 .. Max_Stacks;
   subtype Stack_Index is Stack_Count range 1 .. Stack_Count'Last;
   type Array_Of_Stacks is
     array (Stack_Index) of SK.Stacks.Evaluation_Stack;
   Stacks : Array_Of_Stacks;
   Num_Stacks : Stack_Count := 0;

   GC_Disabled : Natural := 0;

   Protected_Objects : List_Of_Objects.List;

   Tick : Natural := 0;
   GC_Cycle : constant := 2_000_000;
   Collections : Natural := 0;

   Max_Allocation : Object := 0;

   Block_Size : constant := 65536;
   Max_Blocks : constant := 1024;
   --  Up to 512M, or 64M pairs.
   --  That oughta be enough for anybody!  :-)

   Global_Watch : Object;
   --  An object we're watching while garbage collecting
   --  WHAT A HACK!!!!!!!!

   type Array_Of_Pairs is
     array (Object range 0 .. Block_Size - 1) of Object_Pair;
   type Array_Of_Pairs_Access is access Array_Of_Pairs;

   type Array_Of_Markers is
     array (Object range 0 .. Block_Size - 1) of Boolean;
   pragma Pack (Array_Of_Markers);

   type Array_Of_Markers_Access is
     access Array_Of_Markers;

   type Block_Record is
      record
         Pairs    : Array_Of_Pairs_Access;
         Markers  : Array_Of_Markers_Access;
      end record;

   type Array_Of_Blocks is
     array (Object range 0 .. Max_Blocks - 1) of Block_Record;

   type Pool_Index is range 0 .. 1;
   Pool : array (Pool_Index) of Array_Of_Blocks;
   Current_Pool : Pool_Index := 0;
   Pool_Top     : array (Pool_Index) of Object := (others => 0);

   procedure Set_Car (In_Pool : Pool_Index;
                      Item    : Object;
                      To      : Object);

   procedure Set_Cdr (In_Pool : Pool_Index;
                      Item    : Object;
                      To      : Object);

   function Get_Block (From : Object) return Object;
   function Get_Offset (From : Object) return Object;

   procedure Block_Error (Block_No : Object;
                          Item     : Object);

   procedure Allocate (Result :    out Object;
                       Class  : in     Object;
                       From   : in out Array_Of_Blocks;
                       Top    : in out Object);

   function Mark_Tree (Top : Object) return Object;

   procedure Dump_Memory;

   --------------
   -- Allocate --
   --------------

   procedure Allocate (Result :    out Object;
                       Class  : in     Object;
                       From   : in out Array_Of_Blocks;
                       Top    : in out Object)
   is
   begin
      Result := Top * (2**Pointer_Type_Bits);
      if From (Top / Block_Size).Pairs = null then
         From (Top / Block_Size).Pairs := new Array_Of_Pairs;
         From (Top / Block_Size).Markers :=
           new Array_Of_Markers'(others => False);
      end if;
      Top := Top + 1;

      Result := (Result and not Object_Type_Mask) or
        (Class and Object_Type_Mask);

      if Result > Max_Allocation then
         Max_Allocation := Result;
      end if;

   end Allocate;

   --------------
   -- Allocate --
   --------------

   function Allocate (Class : Object) return Object is
      use List_Of_Objects;
      Result : Object;
   begin

      Tick := Tick + 1;
      if Tick >= GC_Cycle and GC_Disabled = 0 then
         Tick := 0;
         Collect;
--           if Length (Free_Objects) < Length (Allocated_Objects) / 10 then
--              Collect;
--           end if;
      end if;

      Allocate (Result, Class, Pool (Current_Pool), Pool_Top (Current_Pool));
      return Result;

   end Allocate;

   -----------------
   -- Block_Error --
   -----------------

   procedure Block_Error (Block_No : Object;
                          Item     : Object)
   is
   begin
      Ada.Text_IO.Put_Line ("Error: attempt to access non-existent block "
                            & Object'Image (Block_No) &
                            " by object" &
                            Object'Image (Item));
   end Block_Error;

   ---------
   -- Car --
   ---------

   function Car (Item : Object) return Object is
      Block    : Array_Of_Blocks renames Pool (Current_Pool);
      Clean    : constant Object := Item and (2**(Object_Bits - 1) - 1);
      Block_No : constant Object := Get_Block (Clean);
      Offset   : constant Object := Get_Offset (Clean);
   begin
      pragma Assert (not Is_Immediate (Item));
      if Block (Block_No).Pairs = null then
         Block_Error (Block_No, Item);
         return Garbage_Object;
      else
         return Block (Block_No).Pairs (Offset).Left;
      end if;
   end Car;

   ---------
   -- Cdr --
   ---------

   function Cdr (Item : Object) return Object is
      Block      : Array_Of_Blocks renames Pool (Current_Pool);
      Clean    : constant Object := Item and (2**(Object_Bits - 1) - 1);
      Block_No   : constant Object := Get_Block (Clean);
      Offset     : constant Object := Get_Offset (Clean);
   begin
      if Block (Block_No).Pairs = null then
         Block_Error (Block_No, Item);
         return Garbage_Object;
      else
         return Block (Block_No).Pairs (Offset).Right;
      end if;
   end Cdr;

   -------------
   -- Collect --
   -------------

   procedure Collect (Watch : in out Object) is
      use List_Of_Objects;
      It : Iterator;
      Block : Array_Of_Blocks renames Pool (Current_Pool);
   begin

      Global_Watch := Watch;

      if Debug_GC then
         Ada.Text_IO.Put_Line ("moving from pool" & Current_Pool'Img &
                               " to pool" &
                               Pool_Index'Image (1 - Current_Pool));
         Ada.Text_IO.Put_Line ("Top      :" &
                               Object'Image (Pool_Top (Current_Pool)));
         Ada.Text_IO.Put_Line ("Protected:" &
                               Natural'Image (Length (Protected_Objects)));
      end if;

      if Debug_Memory then
         Dump_Memory;
      end if;

      for I in Block'Range loop
         exit when Block (I).Markers = null;

         Block (I).Markers.all := (others => False);
      end loop;

      Pool_Top (1 - Current_Pool) := 0;

      It := Get_Start (Protected_Objects);
      while not Off_Right (It) loop
         if Debug_GC_Details then
            Ada.Text_IO.Put_Line ("Protected: " &
                                  SK.Images.Image (Current (It)));

            Ada.Text_IO.Put_Line ("Protected: " &
                                  SK.Images.Low_Level_Image (Current (It)));
         end if;

         declare
            New_Value : constant Object := Mark_Object (Current (It));
         begin
            Replace (It, New_Value);
         end;
         Next (It);
      end loop;

      if Debug_GC_Details then
         Ada.Text_IO.Put_Line ("Number of stacks:" &
                               Num_Stacks'Img);
      end if;

      for I in 1 .. Num_Stacks loop
         SK.Stacks.Operate (Stacks (I), Mark_Object'Access, True);
      end loop;

      Current_Pool := 1 - Current_Pool;

      if Debug_GC_Details then
         for I in 1 .. Num_Stacks loop
            SK.Stacks.Dump_Stack (Stacks (I), True);
         end loop;
      end if;

      if Debug_Gc_Details then
         for I in 0 .. Pool_Top (Current_Pool) loop
            declare
               Item     : constant Object := I * 8;
               Block    : Array_Of_Blocks renames Pool (Current_Pool);
               Block_No : constant Object := Get_Block (Item);
               Offset   : constant Object := Get_Offset (Item);
               Left     : constant Object :=
                 Block (Block_No).Pairs (Offset).Left;
               Right     : constant Object :=
                 Block (Block_No).Pairs (Offset).Right;
            begin
               Ada.Text_IO.Put_Line (SK.Images.Hex_Image (Item) &
                                     ": " &
                                     SK.Images.Hex_Image (Left) & ' ' &
                                     SK.Images.Hex_Image (Right));
            end;
         end loop;
      end if;



--        It := Get_Start (Allocated_Objects);
--        while not Off_Right (It) loop
--           declare
--              Tmp : constant Object := Current (It);
--           begin
--              if not Block (Get_Block (Tmp)).Markers (Get_Offset (Tmp)) then
--                 if Debug_GC_Details then
--                    Ada.Text_IO.Put_Line ("Free: " & Object'Image (Tmp) &
--                                          ": " & SK.Images.Image (Tmp));
--                    Ada.Text_IO.Put_Line
--                      ("Free: " & Object'Image (Tmp) &
--                       ": " & SK.Images.Low_Level_Image (Tmp));
--                 end if;
--                 Append (Free_Objects, Tmp);
--                 Set_Car (Tmp, Garbage_Object);
--                 Set_Cdr (Tmp, Garbage_Object);
--                 Delete (It);
--                 Freed := Freed + 1;
--              else
--                 Next (It);
--              end if;
--           end;
--        end loop;

      if Debug_GC then
         Ada.Text_IO.Put_Line ("New top is" &
                               Object'Image (Pool_Top (Current_Pool)));
      end if;

      if Debug_Gc_Details then
         It := Get_Start (Protected_Objects);
         while not Off_Right (It) loop
            Ada.Text_IO.Put_Line ("Protected: " &
                                  SK.Images.Low_Level_Image (Current (It)));
            Ada.Text_IO.Put_Line ("Protected: " &
                                  SK.Images.Image (Current (It)));

            Next (It);
         end loop;
      end if;

      if Debug_Memory then
         Dump_Memory;
      end if;

      Collections := Collections + 1;
      Watch := Global_Watch;

      Tick := 0;

   end Collect;

   -------------
   -- Collect --
   -------------

   procedure Collect is
      Unused : Object := 0;
   begin
      Collect (Unused);
   end Collect;

   --------------
   -- Debug_GC --
   --------------

   function Debug_GC return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug_Class.GC);
   end Debug_GC;

   ----------------------
   -- Debug_GC_Details --
   ----------------------

   function Debug_GC_Details return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug_Class.GC_Details);
   end Debug_GC_Details;

   ------------------
   -- Debug_Images --
   ------------------

   function Debug_Images return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug_Class.Images);
   end Debug_Images;

   ------------------
   -- Debug_Memory --
   ------------------

   function Debug_Memory return Boolean is
   begin
      return SK.Debug.Enabled (SK.Debug_Class.Memory);
   end Debug_Memory;

   -----------------
   -- Dump_Memory --
   -----------------

   procedure Dump_Memory is
      Current_Top : Object renames Pool_Top (Current_Pool);
      Blocks      : Array_Of_Blocks renames Pool (Current_Pool);
   begin
      Ada.Text_IO.Put_Line ("Memory dump; top = " & Show_Hex (Current_Top));
      Ada.Text_IO.Put_Line ("---------------------------");
      for I in Object range 0 .. Current_Top loop
         Ada.Text_IO.Put (Show_Hex (I) & ": ");
         declare
            Item : constant Object_Pair :=
              Blocks (I / Block_Size).Pairs (I mod Block_Size);
         begin
            Ada.Text_IO.Put (SK.Images.Image (Item.Left));
            Ada.Text_IO.Put (" | ");
            Ada.Text_IO.Put_Line (SK.Images.Image (Item.Right));
         end;
      end loop;
      Ada.Text_IO.Put_Line ("---------------------------");
   end Dump_Memory;

   ----------------
   -- Disable_GC --
   ----------------

   procedure Disable_GC is
   begin
      GC_Disabled := GC_Disabled + 1;
   end Disable_GC;

   ---------------
   -- Enable_GC --
   ---------------

   procedure Enable_GC is
   begin
      GC_Disabled := GC_Disabled - 1;
--        if GC_Disabled = 0 then
--           Collect;
--        end if;
   end Enable_GC;

   ------------------
   -- Export_Image --
   ------------------

   function  Export_Image return Array_Of_Objects is
      Result : Array_Of_Objects
        (1 .. Positive (Pool_Top (Current_Pool) * 2 + 1));
   begin
      for I in Result'Range loop
         declare
            Obj : constant Object :=
              Object ((I - 1) / 2 * (2**Pointer_Type_Bits) + 2);
         begin
            if I mod 2 = 1 then
               Result (I) := Car (Obj);
            else
               Result (I) := Cdr (Obj);
               if Debug_Images then
                  Ada.Text_IO.Put_Line (Show_Hex (Obj) &
                                        ": " &
                                        Show_Hex (Car (Obj)) &
                                        " " &
                                        Show_Hex (Cdr (Obj)));
               end if;
            end if;
            while Is_Symbol (Result (I)) loop
               if Debug_Images then
                  Ada.Text_IO.Put_Line ("Mapping symbol" &
                                        Symbol_Id'Image
                                        (Get_Symbol_Id (Result (I))) &
                                        " " & Get_Name (Result (I)));
                  Ada.Text_IO.Put_Line (" to " &
                                        SK.Images.Image
                                        (Environments.Get_Definition
                                         (Result (I))));
               end if;
               Result (I) := Environments.Get_Definition (Result (I));
            end loop;
         end;
      end loop;
      return Result;
   end Export_Image;

   ---------------
   -- Get_Block --
   ---------------

   function Get_Block (From : Object) return Object is
   begin
      return From / (2**Pointer_Type_Bits) / Block_Size;
   end Get_Block;

   -----------------------------
   -- Get_Max_Allocated_Pairs --
   -----------------------------

   function Get_Max_Allocated_Pairs return Object is
   begin
      return Max_Allocation / 8;
   end Get_Max_Allocated_Pairs;

   -------------------------------
   -- Get_Number_Of_Collections --
   -------------------------------

   function Get_Number_Of_Collections return Natural is
   begin
      return Collections;
   end Get_Number_Of_Collections;

   ----------------
   -- Get_Offset --
   ----------------

   function Get_Offset (From : Object) return Object is
   begin
      return From / (2**Pointer_Type_Bits) mod Block_Size;
   end Get_Offset;

   --------------
   -- Get_Pair --
   --------------

   function Get_Pair (Item : Object) return Object_Pair is
   begin
      pragma Assert (not Is_Immediate (Item));
      return Pool (Current_Pool) (Get_Block (Item)).Pairs (Get_Offset (Item));
   end Get_Pair;

   ------------------
   -- Import_Image --
   ------------------

   procedure Import_Image (Image : Array_Of_Objects) is
      Pair : Object_Pair;
   begin
      Pool_Top (Current_Pool) := 0;
      List_Of_Objects.New_List (Protected_Objects);
      Num_Stacks := 0;

      for I in Image'Range loop
         if I mod 2 = Image'First mod 2 then
            Pair.Left := Image (I);
         else
            Pair.Right := Image (I);
            declare
               New_Object : Object;
            begin
               Allocate (New_Object, 2, Pool (Current_Pool),
                         Pool_Top (Current_Pool));
               Set_Car (New_Object, Pair.Left);
               Set_Cdr (New_Object, Pair.Right);
               if Debug_GC then
                  Ada.Text_IO.Put_Line (Show_Hex (New_Object) &
                                        ": " &
                                        Show_Hex (Pair.Left) &
                                        " " &
                                        Show_Hex (Pair.Right));
               end if;
            end;
         end if;
      end loop;
   end Import_Image;

   -----------------
   -- Mark_Object --
   -----------------

   function Mark_Object (Top : Object) return Object is
   begin
      if Is_Immediate (Top) then
         return Top;
      else
         return Mark_Tree (Top);
      end if;
   end Mark_Object;

   ---------------
   -- Mark_Tree --
   ---------------

   function Mark_Tree (Top : Object) return Object is
      pragma Assert (not Is_Immediate (Top));
      Block  : Array_Of_Blocks renames Pool (Current_Pool);
      Other  : Array_Of_Blocks renames Pool (1 - Current_Pool);
      Marked : Boolean renames
        Block (Get_Block (Top)).Markers (Get_Offset (Top));
      New_Top : Object renames Pool_Top (1 - Current_Pool);
      Result  : Object;
   begin
      if not Marked then
         if Debug_GC_Details then
            Ada.Text_IO.Put_Line ("Marking: " & Object'Image (Top) &
                                  ": " & SK.Images.Image (Top));
            Ada.Text_IO.Put_Line ("Marking: " & Object'Image (Top) &
                                  ": " & SK.Images.Low_Level_Image
                                  (Top));
         end if;
         Marked := True;

         Allocate (Result, Top, Other, New_Top);

         if Debug_Gc_Details then
            Ada.Text_IO.Put_Line ("Marking: " &
                                  Sk.Images.Hex_Image (Top) &
                                  " and moving to " &
                                  SK.Images.Hex_Image (Result));
         end if;

         Set_Car (1 - Current_Pool, Result, Get_Left (Top));
         Set_Cdr (1 - Current_Pool, Result, Get_Right (Top));

         if Top = Global_Watch then
            Global_Watch := Result;
            if Debug_GC then
               Ada.Text_IO.Put_Line ("Replacing watch " &
                                     Show_Hex (Top) &
                                     " with " &
                                     Show_Hex (Result));
            end if;
         end if;

         if Is_User_Defined (Top) then
            SK.Interfaces.Mark
              (SK.Interfaces.From_Object (Top));
         elsif Is_Float (Top) then
            null;
         elsif Is_Application (Top) or Is_Lambda (Top) or
           Is_Pair (Top)
         then
            declare
               Old_Car : constant Object := Car (Top);
               Old_Cdr : constant Object := Cdr (Top);
               New_Car : constant Object := Mark_Object (Car (Top));
               New_Cdr : constant Object := Mark_Object (Cdr (Top));
            begin
               if Debug_GC_Details then
                  Ada.Text_IO.Put_Line ("Old car = " &
                                        SK.Images.Hex_Image (Old_Car) &
                                        "; new car = " &
                                        SK.Images.Hex_Image (New_Car));

                  Ada.Text_IO.Put_Line ("Old cdr = " &
                                        SK.Images.Hex_Image (Old_Cdr) &
                                        "; new cdr = " &
                                        SK.Images.Hex_Image (New_Cdr));
               end if;

               Set_Car (1 - Current_Pool, Result, New_Car);
               Set_Cdr (1 - Current_Pool, Result, New_Cdr);
            end;

            --  Leave a calling card so that we can
            --  follow Top to the new value
            Set_Left (Top, Result);

         end if;

      else
         if Debug_GC_Details then
            Ada.Text_IO.Put_Line ("Moving " & Sk.Images.Hex_Image (Top) &
                                  " to " &
                                  Sk.Images.Hex_Image (Get_Left (Top)));
         end if;

         Result := Get_Left (Top);
      end if;
      return Result;
   end Mark_Tree;

   ---------------
   -- New_Stack --
   ---------------

   function New_Stack return SK.Stacks.Evaluation_Stack is
   begin
      Num_Stacks := Num_Stacks + 1;
      Stacks (Num_Stacks) := SK.Stacks.New_Stack;
      return Stacks (Num_Stacks);
   end New_Stack;

   -------------
   -- Protect --
   -------------

   procedure Protect (Item : Object) is
   begin
      if not Is_Immediate (Item) then
         List_Of_Objects.Append (Protected_Objects, Item);
      end if;
   end Protect;

   -------------
   -- Set_Car --
   -------------

   procedure Set_Car (In_Pool : Pool_Index;
                      Item    : Object;
                      To      : Object)
   is
      Block : Array_Of_Blocks renames Pool (In_Pool);
      Clean : constant Object := Item and (2**(Object_Bits - 1) - 1);
   begin
      pragma Assert (not Is_Immediate (Item));
--        if Item / 16 in 13 .. 14 then
--           Ada.Text_IO.Put_Line ("Pool" & In_Pool'Img &
--                                 ": setting car of " &
--                                 Show_Hex (Item) & " to " &
--                                 SK.Images.Cell_Image (To));
--        end if;
--        if Item = To then
--           Ada.Text_IO.Put_Line ("This might be a problem: " &
--                                 Show_Hex (Item)  &
--                                 " := " & SK.Images.Cell_Image (To));
--        end if;
      Block (Get_Block (Clean)).Pairs (Get_Offset (Clean)).Left := To;
   end Set_Car;

   -------------
   -- Set_Car --
   -------------

   procedure Set_Car (Item : Object;
                      To   : Object)
   is
   begin
      Set_Car (Current_Pool, Item, To);
   end Set_Car;

   -------------
   -- Set_Cdr --
   -------------

   procedure Set_Cdr (In_Pool : Pool_Index;
                      Item    : Object;
                      To      : Object)
   is
      Block : Array_Of_Blocks renames Pool (In_Pool);
      Clean : constant Object := Item and (2**(Object_Bits - 1) - 1);
   begin
      pragma Assert (not Is_Immediate (Clean));
--        if Item / 16 in 13 .. 14 then
--           Ada.Text_IO.Put_Line ("Pool" & In_Pool'Img &
--                                 ": setting cdr of " &
--                                 Show_Hex (Item) & " to " &
--                                 SK.Images.Cell_Image (To));
--        end if;
      Block (Get_Block (Clean)).Pairs (Get_Offset (Clean)).Right := To;
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line ("Bad object: " & Show_Hex (Item));
         Ada.Text_IO.Put_Line ("  in call Set_Cdr" & In_Pool'Img &
                               " " & Show_Hex (Item) & " " &
                               Show_Hex (To));
   end Set_Cdr;

   -------------
   -- Set_Cdr --
   -------------

   procedure Set_Cdr (Item : Object;
                      To   : Object)
   is
   begin
      Set_Cdr (Current_Pool, Item, To);
   end Set_Cdr;

   --------------
   -- Show_Hex --
   --------------

   function Show_Hex (Value : Object) return String is
      Digit : constant String := "0123456789ABCDEF";
      S : String (1 .. 8);
      It : Object := Value;
   begin
      for I in reverse S'Range loop
         S (I) := Digit (Positive (It mod 16 + 1));
         It := It / 16;
      end loop;
      return S(1 .. 4) & "_" & S (5 .. 8);
   end Show_Hex;

   --------------
   -- Set_Pair --
   --------------

   procedure Set_Pair (Item : Object;
                       To   : Object_Pair)
   is
      Block : Array_Of_Blocks renames Pool (Current_Pool);
      Clean : constant Object := Item and (2**(Object_Bits - 1) - 1);
   begin
      pragma Assert (not Is_Immediate (Clean));
      Block (Get_Block (Clean)).Pairs (Get_Offset (Clean)) := To;
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line ("Bad object: " & Show_Hex (Item));
         Ada.Text_IO.Put_Line ("  in call Set_Cdr" & Current_Pool'Img &
                               " " & Show_Hex (Item) & " " &
                               Show_Hex (To.Left) & ":" &
                               Show_Hex (To.Right));
   end Set_Pair;


   ---------------
   -- Unprotect --
   ---------------

   procedure Unprotect (Item : Object) is
      use List_Of_Objects;
      It : Iterator := Get_Start (Protected_Objects);
   begin
      if not Is_Immediate (Item) then
         while not Off_Right (It) loop
            if Current (It) = Item then
               Delete (It);
               return;
            end if;
            Next (It);
         end loop;
      end if;
   end Unprotect;

end SK.Allocation;
