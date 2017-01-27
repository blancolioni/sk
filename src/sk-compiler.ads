with SK.Objects;

private package SK.Compiler is

   function Compile
     (Store : in out SK.Objects.Object_Store'Class;
      Item  : SK.Objects.Object)
      return SK.Objects.Object;

end SK.Compiler;
