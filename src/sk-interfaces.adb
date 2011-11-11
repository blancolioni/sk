with Ada.Tags;
with Ada.Unchecked_Conversion;

with SK.Allocation;
with SK.Functions;

package body SK.Interfaces is

   -------------
   -- Collect --
   -------------

   procedure Collect (Item : access Root_External_Type) is
      pragma Unreferenced (Item);
   begin
      null;
   end Collect;

   -----------------
   -- From_Object --
   -----------------

   function  From_Object (From : Object)
                         return External_Type
   is
      function Object_To_Tag is
         new Ada.Unchecked_Conversion (Object_Pair, Ada.Tags.Tag);
      function Object_To_Item is
         new Ada.Unchecked_Conversion (Object_Pair, External_Type);
      use type Ada.Tags.Tag;
      Result : External_Type;
   begin
      pragma Assert (Is_User_Defined (From));
      Result := Object_To_Item (Allocation.Get_Pair (Get_Right (From)));
      pragma Assert (Result.all'Tag =
                     Object_To_Tag (Allocation.Get_Pair (Get_Left (From))));
      return Result;
   end From_Object;

   ---------------------
   -- Ground_Argument --
   ---------------------

   function Ground_Argument (Argument : Object) return Object is
   begin
      return SK.Functions.Ground_Argument (Argument);
   end Ground_Argument;

   -----------
   -- Image --
   -----------

   function Image (Item : access Root_External_Type) return String is
   begin
      return "<user-defined:" & Type_Name (Item) & ">";
   end Image;

   ----------
   -- Mark --
   ----------

   procedure Mark (Item : access Root_External_Type) is
      pragma Unreferenced (Item);
   begin
      null;
   end Mark;

   ------------------
   -- New_Function --
   ------------------

   procedure New_Function (Name           : String;
                           Argument_Count : Natural;
                           Handler        : Evaluator;
                           Strict         : Boolean)
   is
   begin
      SK.Functions.Add_External_Function (Name, Argument_Count,
                                                  Handler, Strict);
   end New_Function;

   -------------------
   -- Pass_Argument --
   -------------------

   function Pass_Argument   (Argument : Object) return Object is
   begin
      return Get_Right (Argument);
   end Pass_Argument;


   ---------------
   -- To_Object --
   ---------------

   function  To_Object (From : access Root_External_Type'Class)
                       return Object
   is
      function Item_To_Object is
         new Ada.Unchecked_Conversion (External_Type, Object_Pair);
      --  Implementation dependent!

      function Tag_To_Object is
         new Ada.Unchecked_Conversion (Ada.Tags.Tag, Object_Pair);
      --  Implementation dependent!

      Result : constant Object :=
        SK.Allocation.Allocate (User_Defined_Type_Class);
      Converted_From : External_Type;
      Encoded_From   : Object_Pair;
   begin
--        Ada.Text_IO.Put_Line ("Object'Size = " & Object'Size'Img);
--        Ada.Text_IO.Put_Line ("External_type'Size = " &
--                              External_Type'Size'Img);

      Converted_From := External_Type (From);
      Encoded_From   := Item_To_Object (Converted_From);

      Set_Left (Result, Make_Pair (Null_Object, Null_Object));
      Allocation.Set_Pair (Get_Left (Result), Tag_To_Object (From.all'Tag));
      Set_Right (Result, Make_Pair (Null_Object, Null_Object));
      Allocation.Set_Pair (Get_Right (Result), Encoded_From);
      return Result;
   end To_Object;

   ---------------
   -- Type_Name --
   ---------------

   function  Type_Name (Item : access Root_External_Type) return String is
      pragma Unreferenced (Item);
   begin
      return "user-defined type with no name";
   end Type_Name;

   --------------------
   -- Type_Interface --
   --------------------

--     package body Type_Interface is

--        My_Type_Id : Type_Id;

--        -----------------
--        -- Get_Type_Id --
--        -----------------

--        function Get_Type_Id return Type_Id is
--        begin
--       return My_Type_Id;
--        end Get_Type_Id;

--        ----------------------
--        -- Is_External_Type --
--        ----------------------

--        function Is_External_Type (Item : Object) return Boolean is
--        begin
--       return Is_User_Defined (Item) and then
--         Get_Type_Id (Get_Left (Item)) = My_Type_Id;
--        end Is_External_Type;

--        ----------------------
--        -- To_External_Type --
--        ----------------------

--        function To_External_Type (Item : Object) return External_Type is
--       function Object_To_Item is
--          new Ada.Unchecked_Conversion (Object, External_Type);
--        begin
--       pragma Assert (Is_User_Defined (Item));
--       pragma Assert (Get_Type_Id (Get_Left (Item)) = My_Type_Id);
--       return Object_To_Item (Get_Right (Item));
--        end To_External_Type;

--        ---------------
--        -- To_Object --
--        ---------------

--        function To_Object (Item : External_Type) return Object is
--       function Item_To_Object is
--          new Ada.Unchecked_Conversion (External_Type, Object);
--       --  Implementation dependent!

--       Result : Object :=
--         SK.Allocation.Allocate (User_Defined_Type_Class);
--        begin
--       Set_Left (Result, To_Object (My_Type_Id));
--       Set_Right (Result, Item_To_Object (Item));
--       return Result;
--        end To_Object;

--     begin
--        My_Type_Id := Next_Type_Id;
--        Next_Type_Id := Next_Type_Id + 1;
--     end Type_Interface;

end SK.Interfaces;


