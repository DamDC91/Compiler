with Ada.Containers.Indefinite_Multiway_Trees;
with Token;

package Syntaxic_Analysis is

   type Node_Type_Enum_Type is (Node_Constant,
                                Node_Minus_U,
                                Node_Not,
                                Node_Address,
                                Node_Dereference,

                                Node_Op_Assignment,
                                Node_Op_Or_Boolean,
                                Node_Op_And_Boolean,
                                Node_Op_Equal_Comparaison,
                                Node_Op_Difference_Comparaison,
                                Node_Op_Greater_than,
                                Node_Op_Greater_Or_Equal_Than,
                                Node_Op_Less_Than,
                                Node_Op_Less_Or_Equal_Than,
                                Node_Op_Add,
                                Node_Op_Sub,
                                Node_Op_Mult,
                                Node_Op_Division,
                                Node_Op_Modulo,

                                Node_Var_Decl,
                                Node_Var_Ref,
                                Node_Seq,

                                Node_Cond,
                                Node_Call,
                                Node_Body_Func,

                                Node_Loop,
                                Node_Break,
                                Node_Continue,
                                Node_Label,
                                Node_Return,
                                Node_Drop,
                                Node_Instruction_Block,
                                Node_Null);

   subtype Operation_Node_Enum_Type is Node_Type_Enum_Type range Node_Op_Or_Boolean .. Node_Op_Modulo;


   type Node_Variant_Type (Node_Type : Node_Type_Enum_Type := Node_Null) is record
      Line      : Positive;
      case Node_Type is
         when Node_Constant =>
            Value : Integer;
         when Node_Var_Decl =>
            Var_Key : Natural; -- The key associated to this variable
         when Node_Var_Ref =>
            Ref_Var_Key : Natural;     -- This node is referenced the variable associated with this key
            Var_Stack_OffSett : Natural; -- This is the offset of variable in the scope
         when Node_Cond =>
            Cond_NB : Positive;
         when Node_Loop | Node_Break | Node_Continue | Node_Label =>
            Loop_NB : Positive;
         when Node_Body_Func =>
            Name_Key : Natural; -- The key associated to this function
            Nb_Var : Natural;   -- Nb of variable declared inside the function
         when Node_Call =>
            Ref_Func_Key : Natural; -- This node is referenced the function associated with this key
         when others => null;
      end case;
   end record;

   function Same_Node (L,R : Node_Variant_Type) return Boolean;

   package Tree is new Ada.Containers.Indefinite_Multiway_Trees (Element_Type => Node_Variant_Type,
                                                                 "="          => Same_Node);


   function G return Tree.Tree;

   type Priority is range 0..8;

   function E (Min_Priority : Priority := Priority (0)) return Tree.Tree;

   function P return Tree.Tree;

   function A return Tree.Tree;

   function S return Tree.Tree;

   function I return Tree.Tree;

   function F return Tree.Tree;

   --function Debug_Print (N : Node_Variant_Type) return string;

   subtype Operation_Token is Token.Token_Type_Enum_Type range Token.Tok_Plus .. Token.Tok_Or_Boolean;

   type Operation_Record_Type is record
      Left_Priority   : Priority;
      Right_Priority  : Priority;
      Node            : Node_Type_Enum_Type;
   end record;


end Syntaxic_Analysis;
