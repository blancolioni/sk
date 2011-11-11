with SK.Memory;

private package SK.GC is

   procedure Collect (Memory : SK.Memory.Memory_Type;
                      Root   : SK.Memory.Cell_Address;
                      Free   : SK.Memory.Cell_Address);

end SK.GC;
