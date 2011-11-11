package body SK.Utilities is

   --------------------
   -- String_To_List --
   --------------------

   function String_To_List
     (Cells : SK.Cells.Managed_Cells;
      Text  : String)
      return Object
   is
      Result : Object := Empty_List_Object;
   begin
      for I in reverse Text'Range loop
         Result := Make_Pair (To_Object (Integer'(Character'Pos (Text (I)))),
                              Result);
      end loop;
      return Result;
   end String_To_List;

   -------------
   -- To_List --
   -------------

   function To_List (Cells : SK.Cells.Managed_Cells;
                     Item  : Array_Of_Objects)
                     return Object
   is
      Result : Object := Empty_List_Object;
   begin
      Disable_GC;
      for I in reverse Item'Range loop
         Result := Make_Pair (Item (I), Result);
      end loop;
      SK.Stacks.Push (Global_Stack, Result);
      Enable_GC;
      SK.Stacks.Pop (Global_Stack, Result);
      return Result;
   end To_List;

end SK.Utilities;
