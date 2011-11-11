package M6502 is

   type Word8 is mod 256;
   for Word8'Size use 8;

   type Word16 is mod 65536;
   for Word16'Size use 16;

   type Address is new Word16;

   procedure Get_Word8 (From  : in     Address;
                        To    :    out Word8);

   procedure Get_Word16 (From  : in     Address;
                         To    :    out Word16);


   procedure Set_Word8 (Addr  : in Address;
                        Value : in Word8);


   procedure Set_Word16 (Addr  : in Address;
                         Value : in Word16);


   procedure Mem_Copy (Source   : in Address;
                       Target   : in Address;
                       Count    : in Word8);

   procedure Panic (Message : String);

end M6502;

