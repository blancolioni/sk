package SK.Machine.Assembler is

   procedure Push (M     : SK_Machine;
                   Value : Integer);
   procedure Push (M     : SK_Machine;
                   Value : String);
   procedure Push (M     : SK_Machine;
                   Item  : Object);
   procedure Apply (M : SK_Machine);
   procedure Lambda (M        : SK_Machine;
                     Variable : String);

end SK.Machine.Assembler;
