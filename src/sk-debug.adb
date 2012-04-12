with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body SK.Debug is

   Debug_Settings : array (Debug_Class) of Boolean := (others => False);

   ------------
   -- Enable --
   ------------

   procedure Enable (Class : Debug_Class) is
   begin
      Debug_Settings (Class) := True;
   end Enable;

   ------------
   -- Enable --
   ------------

   procedure Enable (Classes : String) is
      Current : Natural := Classes'First;
      Next    : Natural := 0;
   begin
      while Next <= Classes'Last loop
         Next := Ada.Strings.Fixed.Index (Classes, ",", Current);
         if Next = 0 then
            Next := Classes'Last + 1;
         end if;
         declare
            Setting : constant String := Classes (Current .. Next - 1);
         begin
            Enable (Debug_Class'Value (Setting));
         exception
            when Constraint_Error =>
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     "warning: no such debug setting '" &
                                       Setting & "'");
         end;
         Current := Next + 1;
      end loop;
   end Enable;

   -------------
   -- Enabled --
   -------------

   function Enabled (Class : Debug_Class) return Boolean is
   begin
      return Debug_Settings (Class);
   end Enabled;

   -----------
   -- Trace --
   -----------

   procedure Trace (Class   : Debug_Class;
                    Message : String)
   is
   begin
      if Enabled (Class) then
         Ada.Text_IO.Put_Line
           (Ada.Characters.Handling.To_Lower (Debug_Class'Image (Class)) &
              ": " & Message);
      end if;
   end Trace;

end SK.Debug;
