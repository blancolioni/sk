with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with SK.Symbols;

package body SK.Environments is

   Debug_Environments : constant Boolean := False;

   type Env_Entry_Record is
      record
         Name    : Unbounded_String;
         Defn    : Object;
         Unsafe  : Boolean;
         Linked  : Boolean;
      end record;

   type Env_Entry is access Env_Entry_Record;

   package Table_Of_Entries is
     new Ada.Containers.Hashed_Maps (Unbounded_String, Env_Entry,
                                     Ada.Strings.Unbounded.Hash,
                                     "=");
   package Vector_Of_Entries is
     new Ada.Containers.Vectors (Positive, Env_Entry);

   type Environment_Record is
      record
         Name       : Unbounded_String;
         Parent     : Environment;
         Entries    : Table_Of_Entries.Map;
         Entry_List : Vector_Of_Entries.Vector;
      end record;

   Top_Level_Env : Environment := null;

   procedure Define (Name     : String;
                     Env      : Environment;
                     Defn     : Object;
                     Unsafe   : Boolean);

   -----------
   -- Close --
   -----------

   procedure Close (Env : in out Environment) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Environment_Record,
                                         Environment);
   begin
      Free (Env);
   end Close;

   ------------
   -- Define --
   ------------

   procedure Define (Env      : Environment;
                     Name     : String;
                     Defn     : Object)
   is
   begin
      Define (Name, Env, Defn, False);
   end Define;

   ------------
   -- Define --
   ------------

   procedure Define (Name     : String;
                     Env      : Environment;
                     Defn     : Object;
                     Unsafe   : Boolean)
   is
      U_Name : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Env.Entries.Contains (U_Name) then
         Env.Entries.Delete (U_Name);
      end if;

      if Debug_Environments then
         Ada.Text_IO.Put_Line ("Defining: " & Name &
                               " (" &
                               Symbol_Id'Image
                                 (SK.Symbols.Get_Symbol_Id (Name)) &
                               ") = " &
                               Hex_Image (Defn));
      end if;

      declare
         New_Defn : constant Env_Entry :=
                      new Env_Entry_Record'(U_Name, Defn, Unsafe, False);
         Id       : constant Symbol_Id :=
                      SK.Symbols.Get_Symbol_Id (Name);
         Index    : constant Positive := Positive (Id);
      begin
         while Env.Entry_List.Last_Index < Index loop
            Env.Entry_List.Append (null);
         end loop;
         Env.Entry_List.Replace_Element (Index, New_Defn);
         Env.Entries.Insert (U_Name, New_Defn);
      end;
   end Define;

   --------------------
   -- Get_Definition --
   --------------------

   function Get_Definition (Env   : Environment;
                            Name  : String)
                           return Object
   is
      U_Name : constant Unbounded_String :=
                 To_Unbounded_String (Name);
   begin
      if Env.Entries.Contains (U_Name) then
         return Env.Entries.Element (U_Name).Defn;
      else
         Ada.Text_IO.Put_Line ("Undefined: " & Name);
         return Undefined_Object;
      end if;
   end Get_Definition;

   --------------------
   -- Get_Definition --
   --------------------

   function Get_Definition (Env       : Environment;
                            Reference : Object)
                            return Object
   is
      Result : Object;
   begin
      pragma Assert (Is_Symbol (Reference));
      Result := Env.Entry_List.Element
        (Positive (Get_Symbol_Id (Reference))).Defn;
      return Result;
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line ("No definition for symbol reference: " &
                               Hex_Image (Reference) &
                               "(" &
                               SK.Symbols.Get_Name
                                 (Get_Symbol_Id (Reference)) & ")");
         raise Evaluation_Error;
   end Get_Definition;


   --------------
   -- Get_Name --
   --------------

   function Get_Name (Env : Environment) return String is
   begin
      return To_String (Env.Name);
   end Get_Name;

   ---------------
   -- Is_Linked --
   ---------------

   function Is_Linked (Env       : Environment;
                       Reference : Object) return Boolean
   is
   begin
      pragma Assert (Is_Symbol (Reference));
      return Env.Entry_List.Element
        (Positive (Get_Symbol_Id (Reference))).Linked;
   end Is_Linked;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment (Parent : Environment := Top_Level_Environment)
                            return Environment
   is
      Result : constant Environment := new Environment_Record;
   begin
      Result.Parent := Parent;
      return Result;
   end New_Environment;

   ------------------------
   -- Replace_Definition --
   ------------------------

   procedure Replace_Definition (Env       : Environment;
                                 Reference : Object;
                                 New_Value : Object)
   is
      Index   : constant Symbol_Id := Get_Symbol_Id (Reference);
      Current : constant Env_Entry :=
                  Env.Entry_List.Element (Positive (Index));
   begin
      Current.Defn := New_Value;
   end Replace_Definition;

   ----------------
   -- Set_Linked --
   ----------------

   procedure Set_Linked (Env       : Environment;
                         Reference : Object)
   is
      Index   : constant Symbol_Id := Get_Symbol_Id (Reference);
      Current : constant Env_Entry :=
                  Env.Entry_List.Element (Positive (Index));
   begin
      Current.Linked := True;
   end Set_Linked;

   ---------------------------
   -- Top_Level_Environment --
   ---------------------------

   function Top_Level_Environment return Environment is
   begin
      if Top_Level_Env = null then
         Top_Level_Env := New_Environment (null);
         Define (Name   => "#I",
                 Env    => Top_Level_Env,
                 Defn   => SK.I,
                 Unsafe => False);
         for I in 2 .. 15 loop
            Define (Name   => "#pick" & Integer'Image (-I),
                    Env    => Top_Level_Env,
                    Defn   => SK.Pick (I),
                    Unsafe => False);
         end loop;
      end if;
      return Top_Level_Env;
   end Top_Level_Environment;


   procedure Update_All (Updater : not null access
                           function (Defn : Object)
                         return Object)
   is
   begin
      for I in 1 .. Top_Level_Env.Entry_List.Last_Index loop
         declare
            D : constant Env_Entry := Top_Level_Env.Entry_List.Element (I);
         begin
            if D /= null then
               if Debug_Environments then
                  Ada.Text_IO.Put (To_String (D.Name) & ": " &
                                   Hex_Image (D.Defn));
               end if;

               D.Defn := Updater (D.Defn);

               if Debug_Environments then
                  Ada.Text_IO.Put_Line (" --> " & Hex_Image (D.Defn));
               end if;

            end if;
         end;
      end loop;
   end Update_All;

end SK.Environments;
