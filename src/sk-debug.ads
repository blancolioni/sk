package SK.Debug is

   type Debug_Class is (GC, GC_Details, Eval, Laziness, Combinators,
                        Abstraction, Optimisation, Functions, Definitions,
                        Compaction, Pattern_Matching, Images, Parser,
                        Memory, Stacks);

   procedure Enable (Class : Debug_Class);
   procedure Enable (Classes : String);

   function Enabled (Class : Debug_Class) return Boolean;

   procedure Trace (Class   : Debug_Class;
                    Message : String);

end SK.Debug;
