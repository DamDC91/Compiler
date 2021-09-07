with Ada.Containers.Indefinite_Multiway_Trees;

package Syntaxic_Analysis is

   type Node_Type_Enum_Type is (Node_Constant,
                                Node_Minus_U,
                                Node_Not,
                                Node_Address,
                                Node_Dereference);




   type Node (Has_Value : Boolean := False) is record
      Node_Type : Node_Type_Enum_Type;
      Line      : Positive;
      case Has_Value is
         when True => Value : Natural;
         when False => null;
      end case;
   end record;

   function Debug_Print (N : Node) return string;

   function Same_Node (L,R : Node) return Boolean;

   package Tree is new Ada.Containers.Indefinite_Multiway_Trees(Element_Type => Node,
                                                                "="          => Same_Node);

   procedure Debug_Print_Tree (T : Tree.Tree);

   function G return Tree.Tree;

   function E return Tree.Tree;

   function P return Tree.Tree;

   function A return Tree.Tree;

   function S return Tree.Tree;

end Syntaxic_Analysis;
