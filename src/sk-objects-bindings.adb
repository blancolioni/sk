with Ada.Containers.Indefinite_Vectors;

package body SK.Objects.Bindings is

   subtype Real_Function_Id is Function_Id range 1 .. Function_Id'Last;

   package Binding_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Real_Function_Id, Bound_Function_Interface'Class);

   Bindings : Binding_Vectors.Vector;

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
