private with Ada.Calendar;
private with Ada.Containers.Vectors;

with SK.Memory;

with SK.Objects;

private with SK.Objects.Symbol_Maps;

package SK.Machine is

   type SK_Machine_Record is limited new SK.Objects.Object_Store
     and SK.Memory.GC_Interface
   with private;

   type SK_Machine is access all SK_Machine_Record'Class;

   function Create
     (Core_Size : Positive)
      return SK_Machine;

   overriding procedure Push
     (Machine : in out SK_Machine_Record;
      Value   : SK.Objects.Object);

   procedure Push
     (Machine     : in out SK_Machine_Record'Class;
      Symbol_Name : String);

   procedure Lambda
     (Machine       : in out SK_Machine_Record'Class;
      Variable_Name : String);
   --  create a lambda binding around the top of stack

   overriding function Pop
     (Machine : in out SK_Machine_Record)
      return SK.Objects.Object;

   overriding function Top
     (Machine : SK_Machine_Record;
      Index   : Positive := 1)
      return SK.Objects.Object;

   overriding procedure Push_Secondary
     (Machine : in out SK_Machine_Record;
      Value   : SK.Objects.Object);

   overriding procedure Pop_Secondary
     (Machine : in out SK_Machine_Record);

   overriding function Top_Secondary
     (Machine : SK_Machine_Record)
      return SK.Objects.Object;

   overriding procedure Apply
     (Machine : in out SK_Machine_Record);

   overriding function Apply
     (Machine  : in out SK_Machine_Record;
      F, X     : SK.Objects.Object)
      return SK.Objects.Object;

   overriding function Left
     (Machine : SK_Machine_Record;
      Value   : SK.Objects.Object)
      return SK.Objects.Object;

   overriding function Right
     (Machine : SK_Machine_Record;
      Value   : SK.Objects.Object)
      return SK.Objects.Object;

   overriding procedure Set_Left
     (Machine     : in out SK_Machine_Record;
      Application : in     SK.Objects.Object;
      New_Left    : in SK.Objects.Object);

   overriding procedure Set_Right
     (Machine     : in out SK_Machine_Record;
      Application : in     SK.Objects.Object;
      New_Right   : in SK.Objects.Object);

   overriding function Evaluate
     (Machine     : in out SK_Machine_Record;
      Expression  : SK.Objects.Object)
      return SK.Objects.Object;

   procedure Evaluate (Machine : in out SK_Machine_Record'Class);

   overriding function Show
     (Machine : in out SK_Machine_Record;
      Value   : SK.Objects.Object)
      return String;

   function Show_Head
     (Machine : in out SK_Machine_Record'Class;
      Value   : SK.Objects.Object)
      return String;

   overriding function Argument_Count
     (Machine : SK_Machine_Record)
      return Natural;

   overriding function Argument
     (Machine : SK_Machine_Record;
      Index   : Positive)
      return SK.Objects.Object
     with Pre => Index <= Machine.Argument_Count;

   procedure Report_Memory
     (Machine : SK_Machine_Record'Class);

   overriding procedure Report_State
     (Machine : in out SK_Machine_Record);

   overriding function Get_External_Object
     (Machine : SK_Machine_Record;
      Item    : SK.Objects.Object)
      return access SK.Objects.External_Object_Interface'Class;

   overriding function Create_External_Reference
     (Machine : in out SK_Machine_Record;
      External : SK.Objects.External_Object_Interface'Class)
      return SK.Objects.Object;

   overriding procedure Mark
     (Machine : in out SK_Machine_Record;
      Start   : in out SK.Objects.Object);

   overriding procedure Define_Symbol
     (Machine : in out SK_Machine_Record;
      Name    : SK.Objects.Symbol_Id;
      Value   : SK.Objects.Object);

   procedure Define_Symbol
     (Machine : in out SK_Machine_Record;
      Name    : String);
   --  defines Name using top of stack

   procedure Bind
     (Machine : in out SK_Machine_Record;
      Name    : String);
   --  compile top of stack, and bind the result to name

   overriding procedure Get_Symbol
     (Machine : SK_Machine_Record;
      Name    : SK.Objects.Symbol_Id;
      Value   : out SK.Objects.Object;
      Found   : out Boolean);

   procedure Compile
     (Machine : in out SK_Machine_Record'Class);
   --  Compile top of stack to an SK expression

   procedure Link
     (Machine : in out SK_Machine_Record'Class);
   --  Link top of stack; i.e. replace symbols with their definitions

private

   type External_Object_Access is
     access all SK.Objects.External_Object_Interface'Class;

   type External_Object_Record is
      record
         External_Object : External_Object_Access;
         Marked          : Boolean;
         Free            : Boolean;
      end record;

   subtype Real_External_Address is
     SK.Objects.External_Object_Id
   range 1 .. SK.Objects.External_Object_Id'Last;

   package External_Object_Vectors is
     new Ada.Containers.Vectors
       (Real_External_Address,
        External_Object_Record);

   type Register is range 0 .. 15;

   type Register_Values is array (Register) of SK.Objects.Object;

   subtype Argument_Index is Positive range 1 .. 32;
   type Argument_Values is array (Argument_Index) of SK.Objects.Object;

   package Environment_Maps is
     new SK.Objects.Symbol_Maps (SK.Objects.Object,
                                   SK.Objects."=");

   package Linked_Maps is
     new SK.Objects.Symbol_Maps (Boolean);

   type SK_Machine_Record is limited new SK.Objects.Object_Store
     and SK.Memory.GC_Interface with
      record
         Core_Size         : Natural;
         Core              : SK.Memory.SK_Memory;
         Environment       : Environment_Maps.Map;
         Linked            : Linked_Maps.Map;
         Stack             : SK.Objects.Object;
         Secondary_Stack   : SK.Objects.Object;
         Control           : SK.Objects.Object;
         Argument_Stack    : SK.Objects.Object;
         R                 : Register_Values     :=
                               (others => SK.Objects.Nil);
         Args              : Argument_Values :=
                               (others => SK.Objects.Nil);
         Arg_Count         : Natural := 0;
         Eval_Time         : Duration := 0.0;
         Start_Eval        : Ada.Calendar.Time;
         Evaluating        : Boolean := False;
         External_Objects  : External_Object_Vectors.Vector;
      end record;

   overriding procedure Before_GC (Machine : in out SK_Machine_Record);
   overriding procedure After_GC (Machine : in out SK_Machine_Record);
   overriding procedure Mark_External_Object
     (Machine : in out SK_Machine_Record;
      External : SK.Objects.External_Object_Id;
      Mark     : not null access
        procedure (X : in out SK.Objects.Object));

   overriding function Apply
     (Machine  : in out SK_Machine_Record;
      F, X     : SK.Objects.Object)
      return SK.Objects.Object
   is (SK.Memory.Allocate (Machine.Core, F, X));

   overriding function Left
     (Machine : SK_Machine_Record;
      Value   : SK.Objects.Object)
      return SK.Objects.Object
   is (Machine.Core.Car (SK.Objects.Get_Address (Value)));

   overriding function Right
     (Machine : SK_Machine_Record;
      Value   : SK.Objects.Object)
      return SK.Objects.Object
   is (Machine.Core.Cdr (SK.Objects.Get_Address (Value)));

   overriding function Argument_Count
     (Machine : SK_Machine_Record)
      return Natural
   is (Machine.Arg_Count);

   overriding function Argument
     (Machine : SK_Machine_Record;
      Index   : Positive)
      return SK.Objects.Object
   is (Machine.Args (Index));

   overriding function Top_Secondary
     (Machine : SK_Machine_Record)
      return SK.Objects.Object
   is (Machine.Left (Machine.Secondary_Stack));

   function Is_Free (Machine : SK_Machine_Record'Class;
                     Address : SK.Objects.Cell_Address)
                     return Boolean;

   function Control_Size_At_Least
     (Machine : SK_Machine_Record'Class;
      Minimum : Natural)
      return Boolean;

   procedure Push_Control
     (Machine : in out SK_Machine_Record'Class;
      Value   : SK.Objects.Object);

   procedure Drop_Control
     (Machine : in out SK_Machine_Record'Class;
      Count   : Positive := 1)
     with Pre => Machine.Control_Size_At_Least (Count);

   function Pop_Control
     (Machine : in out SK_Machine_Record'Class)
      return SK.Objects.Object
     with Pre => Machine.Control_Size_At_Least (1);

   function Top_Control
     (Machine : SK_Machine_Record'Class;
      Index   : Positive := 1)
      return SK.Objects.Object
     with Pre => Machine.Control_Size_At_Least (Index);

end SK.Machine;
