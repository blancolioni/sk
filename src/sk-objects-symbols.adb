with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded.Hash;

package body SK.Objects.Symbols is

   type Symbol_Entry_Record is
      record
         Name    : Ada.Strings.Unbounded.Unbounded_String;
         Id      : SK.Objects.Symbol_Id;
      end record;

   type Symbol_Entry is access Symbol_Entry_Record;

   package Table_Of_Symbols is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
        Element_Type    => Symbol_Entry,
        Hash            => Ada.Strings.Unbounded.Hash,
        Equivalent_Keys => Ada.Strings.Unbounded."=");

   package Vector_Of_Symbols is
     new Ada.Containers.Vectors (Positive, Symbol_Entry);

   Table  : Table_Of_Symbols.Map;
   Vector : Vector_Of_Symbols.Vector;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Id : SK.Objects.Symbol_Id) return String is
   begin
      return Ada.Strings.Unbounded.To_String
        (Vector.Element (Positive (Id)).Name);
   end Get_Name;

   -------------------
   -- Get_Symbol_Id --
   -------------------

   function Get_Symbol_Id (Name : String) return Symbol_Id is
      use Ada.Strings.Unbounded;
      U_Name : constant Unbounded_String := To_Unbounded_String (Name);
   begin
      if Name = "" then
         return 0;
      elsif Table.Contains (U_Name) then
         return Table.Element (U_Name).Id;
      else
         declare
            New_Sym : constant Symbol_Entry := new Symbol_Entry_Record;
         begin
            New_Sym.Name := U_Name;
            New_Sym.Id   := Symbol_Id (Vector.Last_Index + 1);
            Vector.Append (New_Sym);
            Table.Insert (U_Name, New_Sym);
            return New_Sym.Id;
         end;
      end if;
   end Get_Symbol_Id;

end SK.Objects.Symbols;
