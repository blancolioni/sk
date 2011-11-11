with SK.Cells;

private package SK.Lambdas is

   function Get_Variable (Cells  : SK.Cells.Managed_Cells;
                          Lambda : Object)
                          return Object;

   function Get_Variable (Cells  : SK.Cells.Managed_Cells;
                          Lambda : Object)
                          return String;

   function Get_Body (Cells  : SK.Cells.Managed_Cells;
                      Lambda : Object)
                      return Object;

   function Lambda (Cells       : SK.Cells.Managed_Cells;
                    Variable    : String;
                    Lambda_Body : Object)
                    return Object;

end SK.Lambdas;
