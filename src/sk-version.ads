package SK.Version is

   Version_Major    : constant := 0;
   Version_Minor    : constant := 1;
   Version_Release  : constant := 0;
   Build_Number     : constant := 2;

   Version_String   : constant String := "0.1.0";
   Build_String     : constant String := "2";

   Component_Name   : constant String := "SK";

   Long_Version_String : constant String := Component_Name & " version " & Version_String & " (build " & Build_String & ")";

end SK.Version;
