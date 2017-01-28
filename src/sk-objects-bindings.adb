with Ada.Containers.Indefinite_Vectors;

package body SK.Objects.Bindings is

   subtype Real_Function_Id is Function_Id range 1 .. Function_Id'Last;

   package Binding_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Real_Function_Id, Bound_Function_Interface'Class);

   Bindings : Binding_Vectors.Vector;

   type General_Binding is
     new Bound_Function_Interface with
      record
         Arg_Count : Natural;
         Eval      : General_Function_Handler;
      end record;

   overriding function Is_Strict
     (Bound_Function : General_Binding;
      Argument_Index : Positive)
      return Boolean
   is (True);

   overriding function Argument_Count
     (Bound_Function : General_Binding)
      return Natural
   is (Bound_Function.Arg_Count);

   overriding function Evaluate
     (Bound_Function : General_Binding;
      Store          : in out SK.Objects.Object_Store'Class)
      return SK.Objects.Object
   is (Bound_Function.Eval (Store));

   -----------------
   -- Add_Binding --
   -----------------

   function Add_Binding
     (Handler        : General_Function_Handler;
      Argument_Count : Natural)
      return Function_Id
   is
   begin
      return Add_Binding
        (General_Binding'
           (Arg_Count => Argument_Count,
            Eval      => Handler));
   end Add_Binding;

   -----------------
   -- Add_Binding --
   -----------------

   function Add_Binding
     (Bound_Function : Bound_Function_Interface'Class)
      return Function_Id
   is
   begin
      Bindings.Append (Bound_Function);
      return Bindings.Last_Index;
   end Add_Binding;

   -----------------
   -- Get_Binding --
   -----------------

   function Get_Binding
     (Fn : Function_Id)
      return Bound_Function_Interface'Class
   is
      pragma Assert (Fn <= Bindings.Last_Index);
   begin
      return Bindings.Element (Fn);
   end Get_Binding;

end SK.Objects.Bindings;
