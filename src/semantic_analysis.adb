with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Error_Log;
use Error_Log;
with Ada.Exceptions;
package body Semantic_Analysis is
   
   Nb_Var : Natural := 0;
   
   function Get_Nb_Var return Natural is (Nb_Var);
   
   function Hash (A : Natural) return Ada.Containers.Hash_Type is (Ada.Containers.Hash_Type (A)); -- efficient ?
   --function Equal (L,R : Natural) return Boolean is (L.Symbol_Type = R.Symbol_Type and then L.Idx = R.Idx); -- need to check nb args ? don't think so
   
   package Map is new Ada.Containers.Hashed_Maps (Key_Type        => Natural,
                                                  Element_Type    => Symbol,
                                                  Hash            => Hash,
                                                  Equivalent_Keys => "=");
   
   package Vec is new  Ada.Containers.Indefinite_Vectors (Index_Type   => Natural,
                                                          Element_Type => Map.Map,
                                                          "="          => Map."=");
   
   Var_Vec : Vec.Vector;
   procedure AST_Analyse (T : in out Syntaxic_Analysis.Tree.Tree) is
     -- V : constant Lexical_Analysis.Vector_Association_Table.Vector := Lexical_Analysis.Get_Association_Table;
      
      procedure AST_Analyse_Node (C : Syntaxic_Analysis.Tree.Cursor) is
         N : Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.Element (C);
      
      
      begin
         case N.Node_Type is
         when Syntaxic_Analysis.Node_Var_Decl =>

            begin
               Declare_Ident (Var =>(Symbol_Type => Symbol_Var,
                                     Idx => Nb_Var),
                                     Id => N.Var_Key);
            exception
               when e : Compilation_Error =>
                  Error (MSg => Ada.Exceptions.Exception_Message(e),
                         Line => N.Line);
            end;
               
         when Syntaxic_Analysis.Node_Var_Ref =>
            declare
               s : constant Symbol := Search_Ident (Id => N.Ref_Var_Key);
            begin
               if S.Symbol_Type /= Symbol_Var then
                  Error (Msg  => "missing parenthesis in call statement",
                         Line => N.Line);
                  raise Compilation_Error;
               end if;
               N.Var_Stack_Index := S.Idx;
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
            
         when Syntaxic_Analysis.Node_Func =>
            declare
               Nb_Args : constant Natural := Natural (Syntaxic_Analysis.Tree.Child_Count (Syntaxic_Analysis.Tree.First_Child (C)));
            begin
               begin
                  Declare_Ident (Var =>(Symbol_Type => Symbol_Func,
                                        Nb_Args => Nb_Args),
                                 Id => N.Name_Key);
               exception
                  when e : Compilation_Error =>
                     Error (MSg => Ada.Exceptions.Exception_Message(e),
                         Line => N.Line);
               end;
               
               Nb_Var := 0; 
               Add_Scope;
               Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
               Rem_Scope;
               N.Nb_Var := NB_Var - Nb_Args;
               Syntaxic_Analysis.Tree.Replace_Element (Container => T,
                                     Position  => C,
                                     New_Item  => N);
            end;
            
         when Syntaxic_Analysis.Node_Call =>
            declare
               S : constant Symbol := Search_Ident (Id => N.Ref_Func_Key);
            begin
               if S.Symbol_Type = Symbol_Func then
                  raise Compilation_Error with "Invalid call statment, function not declared";
               elsif S.Nb_Args /= Integer (Syntaxic_Analysis.Tree.Child_Count (C)) then
                raise Compilation_Error with "Invalid number of argument";
               end if;
               Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
            exception
               when e : Compilation_Error =>
                  Error (MSg => Ada.Exceptions.Exception_Message(e),
                         Line => N.Line);
            end;
               
         when others =>
            Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
         end case;
      end AST_Analyse_Node;
      
   begin
      AST_Analyse_Node (Syntaxic_Analysis.Tree.First_Child (T.Root));
      --if not Vec.Is_Empty (Var_Vec) then
      --   raise Compilation_Error with "missing curly bracket somewhere";
      --end if;
   end AST_Analyse;

   -- declare a variable
   procedure Declare_Ident (Var : Symbol; Id : Natural) is
      M : Map.Map;
   begin
      if Var_Vec.Is_Empty then
         Var_Vec.Append(M);
      end if;
      M := Var_Vec.Last_Element;
      if M.Contains(Id) then
         raise Compilation_Error with "2 declarations of with the same name, conflict : " & Var.Idx'Image;
      else
         M.Insert (New_Item => Var,
                   Key => Id);
         Var_Vec.Replace_Element(Var_Vec.Last_Index, M);
         Nb_Var := Nb_Var + 1; 
      end if;
   end Declare_Ident;
   
   -- search a variable and return its index
   function Search_Ident (Id : Natural) return Symbol is
      M : Map.Map;
      Last_Idx : Integer;
   begin
      if Vec.Is_Empty (Var_Vec) then
         raise Compilation_Error with "Search in a empty stack";
      end if;
      
      Last_Idx := Vec.Last_Index (Var_Vec);
      
      while Last_Idx >= 0 loop
         M := Var_Vec.Element (Last_Idx);
         if M.Contains(Id) then
            return Map.Element (M, Id);
         end if;
         Last_Idx := Last_Idx - 1; 
      end loop;
      raise Compilation_Error with "this variable is not declare";
   end Search_Ident;
   
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
