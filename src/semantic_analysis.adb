with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Lexical_Analysis;
with Error_Log;
use Error_Log;
with Ada.Exceptions;
package body Semantic_Analysis is
   
   Nb_Var : Natural := 0;
   
   function Get_Nb_Var return Natural is (Nb_Var);
   
   function Hash (A : Symbol) return Ada.Containers.Hash_Type is (Ada.Containers.Hash_Type (A.Idx));
   function Equal (L,R : Symbol) return Boolean is (L.Idx = R.Idx);
   
   package Map is new Ada.Containers.Hashed_Maps (Key_Type        => Symbol,
                                                  Element_Type    => Natural,
                                                  Hash            => Hash,
                                                  Equivalent_Keys => Equal);
   
   package Vec is new  Ada.Containers.Indefinite_Vectors (Index_Type   => Natural,
                                                          Element_Type => Map.Map,
                                                          "="          => Map."=");
   
   Var_Vec : Vec.Vector;
   procedure AST_Analyse (T : in out Syntaxic_Analysis.Tree.Tree) is
      V : constant Lexical_Analysis.Vector_Association_Table.Vector := Lexical_Analysis.Get_Association_Table;
      
      procedure AST_Analyse_Node (C : Syntaxic_Analysis.Tree.Cursor) is
         N : Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.Element (C);
      
      
      begin
         case N.Node_Type is
         when Syntaxic_Analysis.Node_Var_Decl =>

            begin
               Declare_Var (Var =>(Idx => V.Element(N.Var_Key)));
            exception
               when e : Compilation_Error =>
                  Error (MSg => Ada.Exceptions.Exception_Message(e),
                         Line => N.Line);
            end;
               
         when Syntaxic_Analysis.Node_Var_Ref =>
            declare
               s : constant Natural := Search_Var (Var => (Idx => V.Element(N.Ref_Var_Key)));
            begin
               N.Var_Stack_Index := s;
            exception
               when e : Compilation_Error =>
                  Error (MSg => Ada.Exceptions.Exception_Message(e),
                         Line => N.Line);
            end;
            Syntaxic_Analysis.Tree.Replace_Element (Container => T,
                                                    Position  => C,
                                                    New_Item  => N);
         when Syntaxic_Analysis.Node_Instruction_Block =>
            Add_Scope;
            Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
            Rem_Scope;
         when others =>
            Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
         end case;
      end AST_Analyse_Node;
      
   begin
      AST_Analyse_Node (Syntaxic_Analysis.Tree.First_Child (T.Root));
      if not Vec.Is_Empty (Var_Vec) then
         raise Compilation_Error with "missing curly bracket somewhere";
      end if;
   end AST_Analyse;

   -- declare a variable
   procedure Declare_Var (Var : Symbol) is
      M : Map.Map;
   begin
      if Var_Vec.Is_Empty then
         Var_Vec.Append(M);
      end if;
      M := Var_Vec.Last_Element;
      if M.Contains(Var) then
         raise Compilation_Error with "2 declarations of the same variable : " & Var.Idx'Image;
      else
         M.Insert (Var, Nb_Var);
         Var_Vec.Replace_Element(Var_Vec.Last_Index, M);
         Nb_Var := Nb_Var + 1; 
      end if;
   end Declare_Var;
   
   -- search a variable and return its index
   function Search_Var (Var : Symbol) return Natural is
      M : Map.Map;
      Last_Idx : Integer;
   begin
      if Vec.Is_Empty (Var_Vec) then
         raise Compilation_Error with "Search in a empty stack";
      end if;
      
      Last_Idx := Vec.Last_Index (Var_Vec);
      
      while Last_Idx >= 0 loop
         M := Var_Vec.Element (Last_Idx);
         if M.Contains(Var) then
            return Map.Element (M, Var);
         end if;
         Last_Idx := Last_Idx - 1; 
      end loop;
      raise Compilation_Error with "this variable is not declare";
   end Search_Var;
   
   procedure Add_Scope is 
      M : Map.Map;
   begin
      Vec.Append (Var_Vec, M);
   end Add_Scope;
   
   procedure Rem_Scope is
   begin
      if Vec.Is_Empty (Var_Vec) then
         raise Compilation_Error with "removing last scope on a empty stack";
      end if;
      Vec.Delete_Last (Var_Vec);
   end Rem_Scope;

end Semantic_Analysis;
