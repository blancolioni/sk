with Ada.Sequential_IO;
with Ada.Text_IO;

with SK.Allocation;
with SK.Environments;

package body SK.IO is

   package Object_IO is
      new Ada.Sequential_IO (Object);
   use Object_IO;

   ----------
   -- Load --
   ----------

   function Load (File_Name : String) return Object is
      Start, Size : Object;
      File        : File_Type;
   begin
      Open (File, In_File, File_Name);
      Read (File, Start);
      Read (File, Size);
      declare
         Image : Array_Of_Objects (1 .. Positive (Size));
      begin
         for I in Image'Range loop
            Read (File, Image (I));
         end loop;
         SK.Allocation.Disable_GC;
         SK.Allocation.Import_Image (Image);
         SK.Allocation.Protect (Start);
         SK.Allocation.Enable_GC;
         return Start;
      end;
   exception
      when End_Error =>
         Ada.Text_IO.Put_Line (File_Name & " appears to be truncated");
         return Null_Object;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (File_Name : String;
      Start     : Object)
   is
      File      : File_Type;
      New_Start : Object    := Start;
   begin

      --  Start might be a symbol, but we need to start
      --  at an application
      while Is_Symbol (New_Start) loop
         New_Start := SK.Environments.Get_Definition (New_Start);
      end loop;

      SK.Allocation.Protect (Start);
      SK.Allocation.Collect (New_Start);

      declare
         Image : constant Array_Of_Objects := SK.Allocation.Export_Image;
      begin
         Create (File, Out_File, File_Name);
         Write (File, New_Start);
         Write (File, Object (Image'Length));
         for I in Image'Range loop
            Write (File, Image (I));
         end loop;
         Close (File);
      end;
   end Save;

end SK.IO;
