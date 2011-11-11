with M6502;                             use M6502;
with SK64.Memory;

package body SK64 is

   --------------
   -- Allocate --
   --------------

   function Allocate (Class   : Object_Type;
                      Count   : Positive)
                     return Object
   is
      Result : Object;
   begin
      if not SK64.Memory.Can_Allocate (Count) then
         SK64.Memory.Collect;
         if not SK64.Memory.Can_Allocate (Count) then
            SK64.Memory.Panic ("out of memory");
         end if;
      end if;

      Result := Create_Pair (SK64.Memory.Top);
      SK64.Memory.Top := SK64.Memory.Top + 4 * Address (Count);
      return Result;
   end Allocate;

end SK64.Cells;
