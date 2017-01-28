package SK.Objects is

   type Compare_Result is (LT, EQ, GT);

   type Object is private;

   function Nil return Object;
   function Undefined return Object;
   function Error return Object;
   function Dont_Care return Object;
   function Lambda return Object;
   function Fail return Object;
   function Primitive return Object;

   function S return Object;
   function K return Object;
   function I return Object;
   function C return Object;
   function B return Object;

   function Initial_World return Object;
   function Next_World
     (Current_World : Object)
      return Object;

   function Show_Atom (Value : Object) return String;
   --  Returns a text representation of Value.
   --  Non-atomic values and symbols have their hex image returned instead.

   type Symbol_Id is private;

   function Is_Symbol (Item : Object) return Boolean;
   function To_Object (Symbol : Symbol_Id) return Object;
   function To_Symbol (Item : Object) return Symbol_Id
     with Pre => Is_Symbol (Item);

   type Function_Id is private;

   function Show (Id : Function_Id) return String;

   function Is_Function (Item : Object) return Boolean;
   function To_Object (Fn : Function_Id) return Object;
   function To_Function (Item : Object) return Function_Id
     with Pre => Is_Function (Item);

   function Is_Integer (Item : Object) return Boolean;
   function To_Object (Value : Integer) return Object;
   function To_Integer (Item : Object) return Integer
     with Pre => Is_Integer (Item);
   function In_Object_Range (X : Integer) return Boolean
   is (X in Min_Integer .. Max_Integer);

   function Is_Selection (Item : Object) return Boolean;
   function To_Selection_Object (Count : Positive) return Object;
   function Selection_Count (Item : Object) return Positive
     with Pre => Is_Selection (Item);

   type Cell_Address is mod 2 ** 24;

   function Is_Application (Item : Object) return Boolean;

   function Get_Address (Item : Object) return Cell_Address
     with Pre => Is_Application (Item);

   function To_Application
     (Address : Cell_Address)
      return Object
     with Post => Is_Application (To_Application'Result);

   type External_Object_Id is range 0 .. 2 ** 24 - 1;

   function Is_External_Object (Item : Object) return Boolean;
   function To_Object (External : External_Object_Id) return Object;
   function To_External_Object_Id
     (Item : Object)
      return External_Object_Id
     with Pre => Is_External_Object (Item);

   function Hex_Image (Item : Object) return String;

   type External_Object_Interface is interface;

   type Object_Store is limited interface;

   function Name
     (Item : External_Object_Interface)
      return String
      is abstract;

   function Print (Item  : External_Object_Interface;
                   Store : in out Object_Store'Class)
                   return String
                   is abstract;

   function Equal (X, Y  : External_Object_Interface;
                   Store : Object_Store'Class)
                   return Boolean
                   is abstract;

   procedure Mark (Item  : in out External_Object_Interface;
                   Store : in out Object_Store'Class;
                   Mark  : not null access
                     procedure (X : in out Object))
   is null;

   procedure Finalize (Item  : in out External_Object_Interface;
                       Store : in out Object_Store'Class)
   is null;

   function Left (Store : Object_Store;
                  Application  : Object)
                  return Object
   is abstract
     with Pre'Class => Is_Application (Application);

   function Right (Store : Object_Store;
                   Application  : Object)
                   return Object
   is abstract
     with Pre'Class => Is_Application (Application);

   procedure Set_Left
     (Store       : in out Object_Store;
      Application : Object;
      New_Left    : in Object)
   is abstract
     with Pre'Class => Is_Application (Application);

   procedure Set_Right
     (Store   : in out Object_Store;
      Application : Object;
      New_Right   : in Object)
   is abstract
     with Pre'Class => Is_Application (Application);

   function Show (Store    : in out Object_Store;
                  Item     : Object)
                  return String
                  is abstract;

   function Apply
     (Store : in out Object_Store;
      F, X  : Object)
      return Object
      is abstract;

   procedure Apply (Store : in out Object_Store) is abstract;
   --  Pop x then f off the stack.  Push (f x)

   procedure Mark
     (Store : in out Object_Store;
      Start : in out Object)
   is abstract;

   procedure Push_Secondary
     (Store : in out Object_Store;
      Value : Object)
   is abstract;

   procedure Pop_Secondary
     (Store : in out Object_Store)
   is abstract;

   function Pop_Secondary
     (Store : in out Object_Store'Class)
      return Object;

   function Top_Secondary
     (Store : Object_Store)
      return Object
      is abstract;

   procedure Push (Store     : in out Object_Store;
                   Value     : Object)
   is abstract;

   procedure Push
     (Store   : in out Object_Store'Class;
      Symbol  : Symbol_Id);

   procedure Push
     (Store   : in out Object_Store'Class;
      Value   : Integer);

   function Pop
     (Store     : in out Object_Store)
      return Object
      is abstract;

   function Top
     (Store     : Object_Store;
      Index     : Positive := 1)
      return Object
      is abstract;

   procedure Push
     (Store : in out Object_Store'Class;
      Stack : in out Object;
      Value : Object);

   function Pop
     (Store : in out Object_Store'Class;
      Stack : in out Object)
      return Object;

   type Object_Cursor is tagged private;

   function Get (Store : aliased Object_Store'Class;
                 Value : Object)
                 return Object_Cursor;

   function Top_Cursor
     (Store : aliased Object_Store'Class)
      return Object_Cursor;

   function Top_Secondary_Cursor
     (Store : aliased Object_Store'Class) return Object_Cursor;

   function Left (Current : Object_Cursor) return Object_Cursor;
   function Right (Current : Object_Cursor) return Object_Cursor;

   function Has_Value (Current : Object_Cursor) return Boolean;

   function Value (Current : Object_Cursor) return Object;

   function Is_Application (Current : Object_Cursor'Class) return Boolean
   is (Is_Application (Current.Value));

   function "=" (Left : Object_Cursor'Class;
                 Right : Object)
                 return Boolean
   is (Left.Value = Right);

   procedure Push
     (Store   : in out Object_Store'Class;
      Value   : Object_Cursor'Class)
     with Pre => Value.Has_Value;

   function Evaluate
     (Store : in out Object_Store;
      Expr  : Object)
      return Object
      is abstract;

   procedure Define_Symbol
     (Store : in out Object_Store;
      Name  : Symbol_Id;
      Value : Object)
   is abstract;
   --  define a top-level value

   procedure Get_Symbol
     (Store : Object_Store;
      Name  : Symbol_Id;
      Value : out Object;
      Found : out Boolean)
   is abstract;

   function Get_Symbol
     (Store   : Object_Store'Class;
      Name    : Symbol_Id;
      Default : Object := Undefined)
      return Object;

   function Argument_Count (Store : Object_Store)
                            return Natural
                            is abstract;
   --  Number of arguments passed to the current function

   function Argument (Store : Object_Store;
                      Index : Positive)
                      return Object
                      is abstract;

   procedure Report_State (Store : in out Object_Store) is abstract;

   function Get_External_Object
     (Store : Object_Store;
      Item  : Object)
      return access External_Object_Interface'Class
      is abstract;

   function Create_External_Reference
     (Store    : in out Object_Store;
      External : External_Object_Interface'Class)
      return Object
      is abstract;

   procedure Drop
     (Store : in out Object_Store'Class;
      Count : Natural := 1);

private

   type Object_Payload is mod 2 ** Payload_Size;

   type Object_Tag is
     (Integer_Object,
      Immediate_Object,
      Application_Object,
      Unused);

   type Immediate_Object_Range is range 0 .. 2 ** 24 - 1;

   Immediate_Tag_Size : constant := 8 - Tag_Size;

   type Immediate_Tag_Type is mod 2 ** Immediate_Tag_Size;

   Atomic_Immediate_Tag    : constant := 0;
   Symbol_Immediate_Tag    : constant := 32;
   Function_Immediate_Tag  : constant := 33;
   External_Immediate_Tag  : constant := 34;
   Selection_Immediate_Tag : constant := 35;
   World_Immediate_Tag     : constant := 36;

   type Symbol_Id is new Immediate_Object_Range;
   type Function_Id is new Immediate_Object_Range;

   type Object is
      record
         Payload : Object_Payload := 1;
         Tag     : Object_Tag     := Immediate_Object;
      end record
     with Pack, Size => 32;

   function Is_Application (Item : Object) return Boolean
   is (Item.Tag = Application_Object);

   function Get_Address (Item : Object) return Cell_Address
   is (Cell_Address (Item.Payload));

   function To_Application
     (Address : Cell_Address)
      return Object
   is (Object_Payload (Address), Application_Object);

   function Is_Integer (Item : Object) return Boolean
   is (Item.Tag = Integer_Object);

   function To_Object (Value : Integer) return Object
   is (if Value >= 0
       then (Object_Payload (Value), Integer_Object)
       else (Object_Payload (Value + 2 ** Payload_Size), Integer_Object));

   function To_Integer (Item : Object) return Integer
   is (if Item.Payload < 2 ** (Payload_Size - 1)
       then Integer (Item.Payload)
       else Integer (Item.Payload) - 2 ** Payload_Size);

   function Immediate (Payload : Object_Payload) return Object
   is (Payload, Immediate_Object);

   function Immediate
     (Immediate_Payload : Immediate_Object_Range;
      Tag               : Immediate_Tag_Type)
      return Object
   is (Object_Payload (Immediate_Payload) * 2 ** Immediate_Tag_Size
       + Object_Payload (Tag),
       Immediate_Object);

   function Immediate_Tag (Item : Object) return Immediate_Tag_Type
   is (Immediate_Tag_Type (Item.Payload mod Immediate_Tag_Type'Modulus));

   function Immediate_Payload (Item : Object) return Immediate_Object_Range
   is (Immediate_Object_Range (Item.Payload / 2 ** Immediate_Tag_Size));

   function Nil return Object is (Immediate (0));
   function Undefined return Object is (Immediate (1));
   function Error return Object is (Immediate (2));
   function Lambda return Object is (Immediate (3));
   function Fail return Object is (Immediate (4));
   function Dont_Care return Object is (Immediate (5));
   function Primitive return Object is (Immediate (6));

   function S return Object is (Immediate (16));
   function K return Object is (Immediate (17));
   function I return Object is (Immediate (18));
   function C return Object is (Immediate (19));
   function B return Object is (Immediate (20));

   function Initial_World return Object
   is (Immediate (1, World_Immediate_Tag));

   function Next_World
     (Current_World : Object)
      return Object
   is (Immediate (Immediate_Payload (Current_World) + 1,
                  World_Immediate_Tag));

   function Is_Symbol (Item : Object) return Boolean
   is (Item.Tag = Immediate_Object
         and then Immediate_Tag (Item) = Symbol_Immediate_Tag);

   function To_Object (Symbol : Symbol_Id) return Object
   is (Immediate (Immediate_Object_Range (Symbol), Symbol_Immediate_Tag));

   function To_Symbol (Item : Object) return Symbol_Id
   is (Symbol_Id (Immediate_Payload (Item)));

   function Is_External_Object (Item : Object) return Boolean
   is (Item.Tag = Immediate_Object
       and then Immediate_Tag (Item) = External_Immediate_Tag);

   function To_Object (External : External_Object_Id) return Object
   is (Immediate (Immediate_Object_Range (External), External_Immediate_Tag));

   function To_External_Object_Id
     (Item : Object)
      return External_Object_Id
   is (External_Object_Id (Immediate_Payload (Item)));

   function Is_Function (Item : Object) return Boolean
   is (Item.Tag = Immediate_Object
       and then Immediate_Tag (Item) = Function_Immediate_Tag);

   function To_Object (Fn : Function_Id) return Object
   is (Immediate (Immediate_Object_Range (Fn), Function_Immediate_Tag));

   function To_Function
     (Item : Object)
      return Function_Id
   is (Function_Id (Immediate_Payload (Item)));

   function Is_Selection (Item : Object) return Boolean
   is (Item.Tag = Immediate_Object
       and then Immediate_Tag (Item) = Selection_Immediate_Tag);

   function To_Selection_Object (Count : Positive) return Object
   is (Immediate (Immediate_Object_Range (Count), Selection_Immediate_Tag));

   function Selection_Count (Item : Object) return Positive
   is (Positive (Immediate_Payload (Item)));

   type Object_Cursor is tagged
      record
         Store : access constant Object_Store'Class;
         Value : Object;
      end record;

   function Get (Store : aliased Object_Store'Class;
                 Value : Object)
                 return Object_Cursor
   is (Store'Access, Value);

   function Top_Secondary_Cursor
     (Store : aliased Object_Store'Class) return Object_Cursor
   is (Store.Get (Store.Top_Secondary));

   function Top_Cursor
     (Store : aliased Object_Store'Class) return Object_Cursor
   is (Store.Get (Store.Top));

   function Left (Current : Object_Cursor) return Object_Cursor
   is (Current.Store,
       (if Is_Application (Current.Value)
        then Current.Store.Left (Current.Value)
        else Nil));

   function Right (Current : Object_Cursor) return Object_Cursor
   is (Current.Store,
       (if Is_Application (Current.Value)
        then Current.Store.Right (Current.Value)
        else Nil));

   function Has_Value (Current : Object_Cursor) return Boolean
   is (Current.Value /= Nil);

   function Value (Current : Object_Cursor) return Object
   is (Current.Value);

   function Show (Id : Function_Id) return String
   is ("f" & Integer'Image (-Integer (Id)));

end SK.Objects;
