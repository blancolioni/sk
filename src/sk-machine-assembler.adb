with SK.Cells;
with SK.Stack;

package body SK.Machine.Assembler is

   -----------
   -- Apply --
   -----------

   procedure Apply (M : SK_Machine) is
      Cells : constant SK.Cells.Managed_Cells := Get_Cells (M);
      App  : Object;
      X, Y : Object;
   begin
      SK.Cells.Allocate (Cells, O_Application, App);
      Y := SK.Stack.Pop (Cells);
      X := SK.Stack.Pop (Cells);
      SK.Cells.Set_Car (Cells, App, X);
      SK.Cells.Set_Cdr (Cells, App, Y);
      SK.Stack.Push (Cells, App);
   end Apply;

   ------------
   -- Lambda --
   ------------

   procedure Lambda (M        : SK_Machine;
                     Variable : String)
   is
      Cells : constant SK.Cells.Managed_Cells := Get_Cells (M);
      App   : Object;
      X     : Object;
   begin
      SK.Cells.Allocate (Cells, O_Lambda, App);
      X := SK.Stack.Pop (Cells);
      SK.Cells.Set_Car (Cells, App, Get_Symbol (Variable));
      SK.Cells.Set_Cdr (Cells, App, X);
      SK.Stack.Push (Cells, App);
   end Lambda;

   ----------
   -- Push --
   ----------

   procedure Push (M     : SK_Machine;
                   Value : Integer)
   is
   begin
      SK.Stack.Push (Get_Cells (M), To_Object (Value));
   end Push;

   ----------
   -- Push --
   ----------

   procedure Push (M     : SK_Machine;
                   Value : String)
   is
   begin
      SK.Stack.Push (Get_Cells (M), Get_Symbol (Value));
   end Push;

end SK.Machine.Assembler;
