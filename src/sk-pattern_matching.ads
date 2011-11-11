with SK.Stacks;

private package SK.Pattern_Matching is

   procedure Match (Pattern    : in     SK.Object;
                    Eval_Stack : in     SK.Stacks.Evaluation_Stack;
                    Expr_Index : in     Positive;
                    Bindings   : in out SK.Array_Of_Objects;
                    Success    :    out Boolean);

end SK.Pattern_Matching;

