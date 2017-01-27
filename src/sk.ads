package SK is

   Word_Size    : constant := 32;
   Tag_Size     : constant := 2;
   Payload_Size : constant := Word_Size - Tag_Size;

   Max_Integer : constant := 2 ** (Payload_Size - 1) - 1;
   Min_Integer : constant := -2 ** (Payload_Size - 1);

end SK;
