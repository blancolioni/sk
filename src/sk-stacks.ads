private package SK.Stacks is

   Stack_Error : exception;

   type Evaluation_Stack is private;

   function New_Stack return Evaluation_Stack;
   procedure Close_Stack (Stack : in out Evaluation_Stack);

   procedure Push (To_Stack   : in Evaluation_Stack;
                   Item       : in Object);

   procedure Pop (From_Stack : in     Evaluation_Stack;
                  Item       :    out Object);

   function Top (Of_Stack : Evaluation_Stack) return Object;

   function Top (Of_Stack : Evaluation_Stack;
                 Count    : Natural)
                return Array_Of_Objects;

   function Get (From_Stack : Evaluation_Stack;
                 Index      : Positive)
                return Object;

   procedure Put (To_Stack : Evaluation_Stack;
                  Index    : Positive;
                  Value    : Object);

   procedure Drop (Stack   : in Evaluation_Stack;
                   Count   : in Natural          := 1);

   procedure Set_Boundary (Stack : in out Evaluation_Stack);
   procedure Clear_Boundary (Stack : in out Evaluation_Stack);

   function Is_Empty (Stack : Evaluation_Stack) return Boolean;
   function Size (Stack : Evaluation_Stack) return Natural;

   type Stack_Operator is
     access procedure (Item : Object);

   procedure Operate (Stack          : in Evaluation_Stack;
                      Operator       : in Stack_Operator;
                      Cross_Boundary : in Boolean);

   type Stack_Transformer is
     access function (Item : Object) return Object;

   procedure Operate (Stack          : in Evaluation_Stack;
                      Operator       : in Stack_Transformer;
                      Cross_Boundary : in Boolean);

   function Get_Contents (Stack : Evaluation_Stack)
                         return Array_Of_Objects;

   function Get_Max_Stack_Depth return Natural;

   procedure Dump_Stack (Stack          : Evaluation_Stack;
                         Cross_Boundary : Boolean);

private

   subtype Stack_Count is Natural;
   subtype Stack_Index is Stack_Count range 1 .. Stack_Count'Last;

   type Evaluation_Stack_Record;
   type Evaluation_Stack is access Evaluation_Stack_Record;

   package Array_Of_Stacked_Objects is
      new WL.Arrays.Dynamic (Positive, Object);

   type Evaluation_Stack_Record is
      record
         Stack         : Array_Of_Stacked_Objects.Dynamic_Array;
         Top           : Stack_Count := 0;
         Last_Boundary : Stack_Count := 0;
         Boundaries    : Stack_Count := 0;
      end record;

   pragma Inline (Pop);
   pragma Inline (Push);

end SK.Stacks;

