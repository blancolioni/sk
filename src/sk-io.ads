package SK.IO is

   procedure Save (File_Name : String;
                   Start     : Object);

   function Load (File_Name : String) return Object;

end SK.IO;

