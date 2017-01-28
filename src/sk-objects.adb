package body SK.Objects is

   ----------
   -- Drop --
   ----------

   procedure Drop
     (Store : in out Object_Store'Class;
      Count : Natural := 1)
   is
      V : Object;
      pragma Unreferenced (V);
   begin
      for I in 1 .. Count loop
         V := Store.Pop;
      end loop;
   end Drop;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Store   : Object_Store'Class;
      Name    : Symbol_Id;
      Default : Object := Undefined)
      return Object
   is
      Found : Boolean;
      Result : Object;
   begin
      Store.Get_Symbol (Name, Result, Found);
      if Found then
         return Result;
      else
         return Default;
      end if;
   end Get_Symbol;

   ---------------
   -- Hex_Image --
   ---------------

   function Hex_Image (Item : Object) return String is
      Payload : Object_Payload := Item.Payload;
      Tag     : constant Character :=
                  (case Item.Tag is
                      when Integer_Object   => 'i',
                      when Immediate_Object =>
                     (if Is_Function (Item)
                      then 'f'
                      elsif Is_External_Object (Item)
                      then 'e'
                      elsif Is_Symbol (Item)
                      then 's'
                      else 'k'),
                      when Application_Object => 'a',
                      when Unused    => '?');

      Result : String (1 .. 8);

      function Hex_Digit
        (Item : Object_Payload)
         return Character;
      --  Item should be in range 0 .. 15

      ---------------
      -- Hex_Digit --
      ---------------

      function Hex_Digit
        (Item : Object_Payload)
         return Character
      is
         Hex_Digits : constant String := "0123456789ABCDEF";
      begin
         return Hex_Digits (Positive (Item + 1));
      end Hex_Digit;

   begin
      for I in reverse Result'Range loop
         Result (I) := Hex_Digit (Payload mod 16);
         Payload := Payload / 16;
      end loop;
      return Result & "-" & Tag;
   end Hex_Image;

   ---------
   -- Pop --
   ---------

   function Pop
     (Store : in out Object_Store'Class;
      Stack : in out Object)
      return Object
   is
   begin
      return X : constant Object := Store.Left (Stack) do
         Stack := Store.Right (Stack);
      end return;
   end Pop;

   -------------------
   -- Pop_Secondary --
   -------------------

   function Pop_Secondary
     (Store : in out Object_Store'Class)
      return Object
   is
   begin
      return X : constant Object := Store.Top_Secondary do
         Store.Pop_Secondary;
      end return;
   end Pop_Secondary;

   ----------
   -- Push --
   ----------

   procedure Push
     (Store   : in out Object_Store'Class;
      Symbol  : Symbol_Id)
   is
   begin
      Store.Push (To_Object (Symbol));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (Store   : in out Object_Store'Class;
      Value   : Integer)
   is
   begin
      Store.Push (To_Object (Value));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (Store   : in out Object_Store'Class;
      Value   : Object_Cursor'Class)
   is
   begin
      Store.Push (Value.Value);
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push
     (Store : in out Object_Store'Class;
      Stack : in out Object;
      Value : Object)
   is
   begin
      Store.Push_Secondary (Value);
      Store.Push (Stack);
      Store.Push (Object'(Store.Top_Secondary));
      Store.Pop_Secondary;
      Store.Apply;
      Stack := Store.Pop;
   end Push;

end SK.Objects;
