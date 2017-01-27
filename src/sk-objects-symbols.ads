package SK.Objects.Symbols is

   function Get_Symbol_Id (Name : String) return Symbol_Id;
   function Get_Object (Name : String) return Object
     with Post => Is_Symbol (Get_Object'Result);

   function Get_Name (Id : Symbol_Id) return String;
   function Get_Name (X : Object) return String
     with Pre => Is_Symbol (X);

private

   function Get_Object (Name : String) return Object
   is (To_Object (Get_Symbol_Id (Name)));

   function Get_Name (X : Object) return String
   is (Get_Name (To_Symbol (X)));

end SK.Objects.Symbols;
