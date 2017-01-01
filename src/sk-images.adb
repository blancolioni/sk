with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with SK.Functions;

package body SK.Images is

   type Image_Type is (Pretty, Low_Level, Pair, Cell);

   function Trim (Image : String) return String;

   function Image (Cells  : SK.Cells.Managed_Cells;
                   Item   : Object;
                   Format : Image_Type;
                   Found  : Array_Of_Objects)
                  return String;

   function Combinator_Image (Item : Combinator) return String;

   Empty_Object_List : Array_Of_Objects (1 .. 0);

   ----------------
   -- Cell_Image --
   ----------------

   function Cell_Image (Cells : SK.Cells.Managed_Cells;
                        Item  : Object)
                        return String
   is
   begin
      return Image (Cells, Item, Pair, Empty_Object_List);
   end Cell_Image;

   ----------------------
   -- Combinator_Image --
   ----------------------

   function Combinator_Image (Item : Combinator) return String is
   begin
      case Item is
         when I => return "I";
         when S => return "S";
         when K => return "K";
         when B => return "B";
         when C => return "C";
         when Sd => return "S'";
         when Bd => return "B*";
         when Cd => return "C'";
      end case;
   end Combinator_Image;

   -----------
   -- Image --
   -----------

   function Image (Cells : SK.Cells.Managed_Cells;
                   Item  : Object)
                   return String
   is
   begin
      return Image (Cells, Item,
                    Format => Pretty,
                    Found  => Empty_Object_List);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Cells  : SK.Cells.Managed_Cells;
                   Item   : Object;
                   Format : Image_Type;
                   Found  : Array_Of_Objects)
                  return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
      function "+" (Item : String) return Unbounded_String
                    renames To_Unbounded_String;

      function Get_Left (Item : Object) return Object;
      function Get_Right (Item : Object) return Object;

      --------------
      -- Get_Left --
      --------------

      function Get_Left (Item : Object) return Object is
      begin
         return SK.Cells.Car (Cells, Item);
      end Get_Left;

      ---------------
      -- Get_Right --
      ---------------

      function Get_Right (Item : Object) return Object is
      begin
         return SK.Cells.Cdr (Cells, Item);
      end Get_Right;

      New_Found : constant Array_Of_Objects := Item & Found;
   begin
      for I in Found'Range loop
         if Item = Found (I) then
            return "<infinite loop: " & Hex_Image (Item) & ">";
         end if;
      end loop;

      if Format = Low_Level then
         if Is_Immediate (Item) then
            Result := +Hex_Image (Item);
         else
            Result := +Hex_Image (Item / 8) & ": (" &
              Image (Cells, Get_Left (Item), Format, New_Found) & " " &
              Image (Cells, Get_Right (Item), Format, New_Found) & ")";
         end if;
      else
         if Format = Pair or else Format = Cell then
            Result := +Hex_Image (Item) & ":";
         end if;
         if Is_Combinator (Item) then
            Result := Result & Combinator_Image (Get_Combinator (Item));
         elsif Is_Integer (Item) then
            declare
               Value : constant Integer := Get_Integer (Item);
               Image : constant String  := Integer'Image (Value);
            begin
               Result := Result & Trim (Image);
            end;
         elsif Item = Undefined_Object then
            Result := Result & "<undefined>";
         elsif Item = Unbound_Object then
            Result := Result & "<unbound>";
         elsif Item = Dont_Care_Object then
            Result := Result & "<don't care>";
         elsif Item = Fail_Object then
            Result := Result & "<fail>";
         elsif Is_Function (Item) then
            Result := Result & "<f:" & SK.Functions.Get_Function_Name
              (Get_Function_Id (Item)) & ">";
         elsif Is_Symbol (Item) then
            Result := Result & Get_Name (Item);
         elsif Is_Application (Item) then
            case Format is
               when Pretty =>
                  if Is_Application (Get_Right (Item)) then
                     Result := Result &
                     Image (Cells, Get_Left (Item), Format, New_Found) & " " &
                     "(" & Image (Cells, Get_Right (Item), Format, New_Found) &
                     ")";
                  else
                     Result := Result &
                     Image (Cells, Get_Left (Item), Format, New_Found) & " " &
                     Image (Cells, Get_Right (Item), Format, New_Found);
                  end if;
               when Pair =>
                  Result := Result &
                  "(" & Image (Cells, Get_Left (Item), Cell, New_Found) & " " &
                  Image (Cells, Get_Right (Item), Cell, New_Found) & ")";
               when Cell =>
                  Result := Result &
                  "--> " & Hex_Image (Item / 4);
               when Low_Level =>
                  null;
            end case;
         elsif Is_Lambda (Item) then
            case Format is
               when Pretty =>
                  Result := Result &
                    "(\" & Image (Cells, Get_Left (Item)) & "." &
                    Image (Cells, Get_Right (Item), Pretty, New_Found) & ')';
               when Pair =>
                  Result := Result &
                    "(\" & Image (Cells, Get_Left (Item)) & "." &
                    Image (Cells, Get_Right (Item), Cell, New_Found);
               when Cell =>
                  Result := Result &
                  "(\" & Image (Cells, Get_Left (Item)) & "." &
                  "--> " & Hex_Image (Item / 4);
               when Low_Level =>
                  null;
            end case;
         elsif Item = Empty_List_Object then
            Result := Result & "[]";
         elsif Is_Null (Item) then
            Result := Result & "<>";
         elsif Is_Pick (Item) then
            Result := Result & "pick" & Integer'Image (-Num_Picks (Item));
         elsif Item = Last_Argument then
            Result := Result & "<last-argument>";
         elsif Item = Next_Argument then
            Result := Result & "<next-argument>";
         else
            Result := Result & "<unknown: " & Hex_Image (Item) & ">";
         end if;
      end if;
      return To_String (Result);
   end Image;

   ---------------------
   -- Low_Level_Image --
   ---------------------

   function Low_Level_Image (Cells : SK.Cells.Managed_Cells;
                             Item  : Object)
                             return String
   is
   begin
      return Image (Cells, Item, Low_Level, Empty_Object_List);
   end Low_Level_Image;

   ----------
   -- Trim --
   ----------

   function Trim (Image : String) return String is
   begin
      return Ada.Strings.Fixed.Trim (Image, Ada.Strings.Both);
   end Trim;

end SK.Images;
