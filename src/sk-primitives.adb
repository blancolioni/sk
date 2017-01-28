with Ada.Text_IO;

with SK.Objects.Bindings;
with SK.Objects.Symbols;

package body SK.Primitives is

   type Strict_Binding is
     abstract new SK.Objects.Bindings.Bound_Function_Interface
   with null record;

   overriding function Is_Strict
     (Bound_Function : Strict_Binding;
      Argument_Index : Positive)
      return Boolean
   is (True);

   type Strict_Infix_Binding is
     abstract new Strict_Binding
   with null record;

   overriding function Argument_Count
     (Bound_Function : Strict_Infix_Binding)
      return Natural
   is (2);

   type Int_To_Int_Evaluator is access
     function (X : Integer) return Integer;

   type Int_To_Int_Binding is
     new Strict_Binding with
      record
         Eval : Int_To_Int_Evaluator;
      end record;

   overriding function Argument_Count
     (Bound_Function : Int_To_Int_Binding)
      return Natural
   is (1);

   overriding function Evaluate
     (Bound_Function : Int_To_Int_Binding;
      Store          : in out SK.Objects.Object_Store'Class)
      return SK.Objects.Object
   is (SK.Objects.To_Object
       (Bound_Function.Eval
        (SK.Objects.To_Integer (Store.Argument (1)))));

   type Int_Int_To_Int_Evaluator is access
     function (X, Y : Integer) return Integer;

   type Int_Int_To_Int_Binding is
     new Strict_Infix_Binding with
      record
         Eval : Int_Int_To_Int_Evaluator;
      end record;

   overriding function Evaluate
     (Bound_Function : Int_Int_To_Int_Binding;
      Store          : in out SK.Objects.Object_Store'Class)
      return SK.Objects.Object
   is (SK.Objects.To_Object
       (Bound_Function.Eval
        (SK.Objects.To_Integer (Store.Argument (1)),
         SK.Objects.To_Integer (Store.Argument (2)))));

   type Int_Int_To_Bool_Evaluator is access
     function (X, Y : Integer) return Boolean;

   type Int_Int_To_Bool_Binding is
     new Strict_Infix_Binding with
      record
         Eval : Int_Int_To_Bool_Evaluator;
      end record;

   overriding function Evaluate
     (Bound_Function : Int_Int_To_Bool_Binding;
      Store          : in out SK.Objects.Object_Store'Class)
      return SK.Objects.Object
   is (SK.Objects.To_Object
       (Integer'
          (Boolean'Pos
           (Bound_Function.Eval
            (SK.Objects.To_Integer (Store.Argument (1)),
             SK.Objects.To_Integer (Store.Argument (2)))))));

   type Value_World_To_World_Evaluator is access
     function (Value, World : SK.Objects.Object) return SK.Objects.Object;

   type Value_World_To_World_Binding is
     new Strict_Infix_Binding with
      record
         Eval : Value_World_To_World_Evaluator;
      end record;

   overriding function Evaluate
     (Bound_Function : Value_World_To_World_Binding;
      Store          : in out SK.Objects.Object_Store'Class)
      return SK.Objects.Object
   is (Bound_Function.Eval (Store.Argument (1), Store.Argument (2)));

   function Eval_Int_Eq
     (X, Y : Integer)
      return Boolean
   is (X = Y);

   function Eval_Int_Neq
     (X, Y : Integer)
      return Boolean
   is (X /= Y);

   function Eval_Int_Plus
     (X, Y : Integer)
      return Integer
   is (X + Y);

   function Eval_Int_Minus
     (X, Y : Integer)
      return Integer
   is (X - Y);

   function Eval_Int_Mult
     (X, Y : Integer)
      return Integer
   is (X * Y);

   function Eval_Int_GT
     (X, Y : Integer)
      return Boolean
   is (X > Y);

   function Eval_Int_LT
     (X, Y : Integer)
      return Boolean
   is (X < Y);

   function Eval_Int_Id
     (X : Integer)
      return Integer
   is (X);

   function Eval_Put_Char
     (Value, World : SK.Objects.Object)
      return SK.Objects.Object;

   procedure Load_Int_Primitives
     (Store : in out SK.Objects.Object_Store'Class);

   -------------------
   -- Eval_Put_Char --
   -------------------

   function Eval_Put_Char
     (Value, World : SK.Objects.Object)
      return SK.Objects.Object
   is
   begin
      Ada.Text_IO.Put (Character'Val (SK.Objects.To_Integer (Value)));
      return SK.Objects.Next_World (World);
   end Eval_Put_Char;

   -------------------------
   -- Load_Int_Primitives --
   -------------------------

   procedure Load_Int_Primitives
     (Store : in out SK.Objects.Object_Store'Class)
   is
      procedure Bind
        (Name : String;
         Eval : Int_To_Int_Evaluator);

      procedure Bind
        (Name : String;
         Eval : Int_Int_To_Int_Evaluator);

      procedure Bind
        (Name : String;
         Eval : Int_Int_To_Bool_Evaluator);

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Name : String;
         Eval : Int_To_Int_Evaluator)
      is
         Binding : constant Int_To_Int_Binding := (Eval => Eval);
         Id      : constant SK.Objects.Function_Id :=
                     SK.Objects.Bindings.Add_Binding
                       (Binding);
      begin
         Store.Define_Symbol
           (SK.Objects.Symbols.Get_Symbol_Id (Name),
            SK.Objects.To_Object (Id));
      end Bind;

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Name : String;
         Eval : Int_Int_To_Int_Evaluator)
      is
         Binding : constant Int_Int_To_Int_Binding := (Eval => Eval);
         Id : constant SK.Objects.Function_Id :=
                SK.Objects.Bindings.Add_Binding
                       (Binding);
      begin
         Store.Define_Symbol
           (SK.Objects.Symbols.Get_Symbol_Id (Name),
            SK.Objects.To_Object (Id));
      end Bind;

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Name : String;
         Eval : Int_Int_To_Bool_Evaluator)
      is
         Binding : constant Int_Int_To_Bool_Binding := (Eval => Eval);
         Id      : constant SK.Objects.Function_Id :=
                     SK.Objects.Bindings.Add_Binding
                       (Binding);
      begin
         Store.Define_Symbol
           (SK.Objects.Symbols.Get_Symbol_Id (Name),
            SK.Objects.To_Object (Id));
      end Bind;

   begin
      Bind ("#intId", Eval_Int_Id'Access);
      Bind ("#intPlus", Eval_Int_Plus'Access);
      Bind ("#intMinus", Eval_Int_Minus'Access);
      Bind ("#intMult", Eval_Int_Mult'Access);
      Bind ("#intEq", Eval_Int_Eq'Access);
      Bind ("#intNeq", Eval_Int_Neq'Access);
      Bind ("#intGT", Eval_Int_GT'Access);
      Bind ("#intLT", Eval_Int_LT'Access);

      Store.Define_Symbol
        (SK.Objects.Symbols.Get_Symbol_Id ("#intLast"),
         SK.Objects.To_Object (Integer'(Max_Integer)));
      Store.Define_Symbol
        (SK.Objects.Symbols.Get_Symbol_Id ("#intFirst"),
         SK.Objects.To_Object (Integer'(Min_Integer)));

      Store.Define_Symbol
        (SK.Objects.Symbols.Get_Symbol_Id ("#fail"),
         SK.Objects.Fail);

      Store.Define_Symbol
        (SK.Objects.Symbols.Get_Symbol_Id ("#initWorld"),
         SK.Objects.Initial_World);

      Store.Define_Symbol
        (SK.Objects.Symbols.Get_Symbol_Id ("#undefined"),
         SK.Objects.Undefined);

      Store.Define_Symbol
        (SK.Objects.Symbols.Get_Symbol_Id ("#I"),
         SK.Objects.I);

      Store.Define_Symbol
        (SK.Objects.Symbols.Get_Symbol_Id ("#S"),
         SK.Objects.S);

      Store.Define_Symbol
        (SK.Objects.Symbols.Get_Symbol_Id ("#K"),
         SK.Objects.K);

   end Load_Int_Primitives;

   ---------------------
   -- Load_Primitives --
   ---------------------

   procedure Load_Primitives
     (Store : in out SK.Objects.Object_Store'Class)
   is
   begin
      Load_Int_Primitives (Store);

      Store.Define_Symbol
        (SK.Objects.Symbols.Get_Symbol_Id ("#putChar"),
         SK.Objects.To_Object
           (SK.Objects.Bindings.Add_Binding
                (Value_World_To_World_Binding'
                     (Eval => Eval_Put_Char'Access))));
   end Load_Primitives;

end SK.Primitives;
