package SK.Interfaces is

   procedure New_Function (Name           : String;
                           Argument_Count : Natural;
                           Handler        : Evaluator;
                           Strict         : Boolean);
   --  Add a new function.  Arguments to the handler might
   --  need to be evaluated.
   --  If Strict is True, then the function will be passed an
   --  array of objects, each containing the value of an argument.
   --  If Strict if False, then the objects in the array cannot
   --  be used directly.  To force evaluation of an argument,
   --  call Ground_Argument and use the result.  To pass the
   --  argument unevaluated to another function, use the result
   --  of the function Pass_Argument.

   function Ground_Argument (Argument : Object) return Object;
   function Pass_Argument   (Argument : Object) return Object;

   type Root_External_Type is abstract tagged private;

   procedure Mark (Item : access Root_External_Type);
   procedure Collect (Item : access Root_External_Type);
   function Image (Item : access Root_External_Type) return String;
   function  Type_Name (Item : access Root_External_Type) return String;

   type External_Type is access all Root_External_Type'Class;

   function  To_Object (From : access Root_External_Type'Class)
                       return Object;
   function  From_Object (From : Object)
                         return External_Type;

--     generic
--        type External_Type is private;
--        Name : String;
--        with function Image (Item : External_Type) return String;
--        with procedure Collect (Item : in out External_Type);
--        with procedure Mark (Item : in External_Type);
--     package Type_Interface is
--        function Get_Type_Id return Type_Id;
--        function Is_External_Type (Item : Object) return Boolean;
--        function To_Object (Item : External_Type) return Object;
--        function To_External_Type (Item : Object) return External_Type;
--     end Type_Interface;

private

   type Root_External_Type is abstract tagged null record;

end SK.Interfaces;


