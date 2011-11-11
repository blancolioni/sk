private package SK.Evaluators is

   type Evaluator is private;

   function Create_Evaluator (Size    : Object)
                           return Evaluator;

   function Allocate (E     : Evaluator;
                      Class : Object)
                      return Object;

   function Car (E    : Evaluator;
                 Item : Object)
                 return Object;

   function Cdr (E    : Evaluator;
                 Item : Object)
                 return Object;

   procedure Set_Car (E    : Evaluator;
                      Item : Object;
                      To   : Object);

   procedure Set_Cdr (E    : Evaluator;
                      Item : Object;
                      To   : Object);

end SK.Evaluators;
