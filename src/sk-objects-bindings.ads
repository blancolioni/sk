package SK.Objects.Bindings is

   type Bound_Function_Interface is interface;

   function Argument_Count
     (Bound_Function : Bound_Function_Interface)
      return Natural
      is abstract;

   function Is_Strict
     (Bound_Function : Bound_Function_Interface;
      Argument_Index : Positive)
      return Boolean
   is abstract
     with Pre'Class =>
       Argument_Index <=
         Bound_Function_Interface'Class (Bound_Function).Argument_Count;

   function Evaluate
     (Bound_Function : Bound_Function_Interface;
      Store          : in out Object_Store'Class)
      return SK.Objects.Object
      is abstract;

   function Add_Binding
     (Bound_Function : Bound_Function_Interface'Class)
      return Function_Id;

   function Get_Binding
     (Fn : Function_Id)
      return Bound_Function_Interface'Class;

end SK.Objects.Bindings;
