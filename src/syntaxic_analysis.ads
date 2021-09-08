with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Hashed_Maps;
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

                                Node_Drop,
                                Node_Debug); -- temp


   type Node_Record_Type (Has_Value : Boolean := False) is record
      Node_Type : Node_Type_Enum_Type;
      Line      : Positive;
      case Has_Value is
         when True => Value : Natural;
         when False => null;
      end case;
   end record;

   function Same_Node (L,R : Node_Record_Type) return Boolean;

   package Tree is new Ada.Containers.Indefinite_Multiway_Trees(Element_Type => Node_Record_Type,
                                                                "="          => Same_Node);

   procedure Debug_Print_Tree (T : Tree.Tree);

   function G return Tree.Tree;

   function E return Tree.Tree;

   function P return Tree.Tree;

   function A return Tree.Tree;

   function S return Tree.Tree;

   procedure Init;

private

   function Debug_Print (N : Node_Record_Type) return string;

   subtype Operation_Token is Token.Token_Type_Enum_Type
   with Static_Predicate => Operation_Token in Token.Tok_Assignment
   | Token.Tok_Or_Boolean
   | Token.Tok_And_Boolean
   | Token.Tok_Equal_Comparaison
   | Token.Tok_Difference_Comparaison
   | Token.Tok_Greater_than
   | Token.Tok_Greater_Or_Equal_Than
   | Token.Tok_Less_Than
   | Token.Tok_Less_Or_Equal_Than
   | Token.Tok_Plus
   | Token.Tok_Minus
   | Token.Tok_Asterisk
   | Token.Tok_Slash
   | Token.Tok_Percent;

   Type Priority is range 0..8;

   type Operation_Record_Type is record
      Left_Priority  : Priority;
      Right_Priority : Priority;
      Node           : Node_Type_Enum_Type;
   end record;

   function Hash (Op : Operation_Token) return Ada.Containers.Hash_Type is (Ada.Containers.Hash_Type (Token.Token_Type_Enum_Type'Pos(op)));

   function Equals_Operation_Token (L,R : Operation_Token) return boolean;

   package Operation_Map is new Ada.Containers.Hashed_Maps (Key_Type        => Operation_Token,
                                                            Element_Type    => Operation_Record_Type,
                                                            Hash            => Hash,
                                                            Equivalent_Keys => Equals_Operation_Token);

end Syntaxic_Analysis;
