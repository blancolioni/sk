with M6502;

package SK64 is

   type Object is new M6502.Word16;

   type Object_Type is (Integer_Object,
                        Primitive_Object,
                        Pair_Object,
                        Undefined_Object_Type);

   function Get_Object_Type (Item : Object) return Object_Type;
   function Is_Integer (Item : Object) return Boolean;
   function Is_Primitive (Item : Object) return Boolean;
   function Is_Pair (Item : Object) return Boolean;

   function Is_Combinator (Item : Object) return Boolean;
   function Is_Function (Item : Object) return Boolean;

   type Combinator is (C_I, C_S, C_K, C_B, C_C, C_Sp, C_Bs, C_Cp);

   function Get_Combinator (Item : Object) return Combinator;

   type Function_Id is new M6502.Word8;

   function Get_Function_Id (Item : Object) return Function_Id;

   function Car (Item : Object) return Object;
   function Cdr (Item : Object) return Object;

   procedure Create_Pair (Item :    out Object;
                          Addr : in     M6502.Address);

   function Get_Address (Item : Object) return M6502.Address;

end SK64;
