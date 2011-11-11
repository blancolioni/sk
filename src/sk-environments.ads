private package SK.Environments is

   type Environment is private;

   function Top_Level_Environment return Environment;

   procedure Define (Env      : Environment;
                     Name     : String;
                     Defn     : Object);


   function Get_Definition (Env   : Environment;
                            Name  : String)
                           return Object;

   function Get_Definition (Env       : Environment;
                            Reference : Object)
                            return Object;
   --  Gets the definition referred to by Reference, which must
   --  be a Symbol


   procedure Replace_Definition (Env       : Environment;
                                 Reference : Object;
                                 New_Value : Object);

   function New_Environment (Parent : Environment := Top_Level_Environment)
                            return Environment;

   function Get_Name (Env : Environment) return String;

   procedure Close (Env : in out Environment);

   function Is_Linked (Env       : Environment;
                       Reference : Object) return Boolean;
   --  return True if the definition of Reference is fully linked

   procedure Set_Linked (Env       : Environment;
                         Reference : Object);

   procedure Update_All (Updater : not null access
                           function
                             (Defn : Object)
                         return Object);

private

   type Environment_Record;

   type Environment is access all Environment_Record;

end SK.Environments;
