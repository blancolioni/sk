with M6502;                             use M6502;

package SK64.Memory is

   function Can_Allocate (Count : Positive) return Boolean;

   procedure Collect;

   procedure Allocate (X, Y    : in out Object;
                       Result  :    out Object);

end SK64.Memory;
