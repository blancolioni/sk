with SK.Cells;

private package SK.Evaluator is

   procedure Evaluate (Cells     : in     SK.Cells.Managed_Cells;
                       Item      : in out Object);

   function Evaluate (Cells     : in SK.Cells.Managed_Cells;
                      Item      : in Object)
                      return Object;

end SK.Evaluator;
