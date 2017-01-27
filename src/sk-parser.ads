with SK.Objects;

package SK.Parser is

   function Parse_String
     (Store : in out SK.Objects.Object_Store'Class;
      S     : String)
      return SK.Objects.Object;

end SK.Parser;
