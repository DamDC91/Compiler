with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Error_Log;
use Error_Log;
with Ada.Exceptions;
with Lexical_Analysis;
with Ada.Containers;
package body Semantic_Analysis is
   
   Nb_Var : Natural := 0;
   
   function Get_Nb_Var return Natural is (Nb_Var);
   
   function Hash (A : Natural) return Ada.Containers.Hash_Type is (Ada.Containers.Hash_Type (A));
   
   package Map is new Ada.Containers.Hashed_Maps (Key_Type        => Natural,
                                                  Element_Type    => Symbol,
                                                  Hash            => Hash,
                                                  Equivalent_Keys => "=");
   
   package Vec is new  Ada.Containers.Indefinite_Vectors (Index_Type   => Natural,
                                                          Element_Type => Map.Map,
                                                          "="          => Map."=");
   
   Var_Vec : Vec.Vector;
   procedure AST_Analyse (T : in out Syntaxic_Analysis.Tree.Tree) is
      
      procedure AST_Analyse_Node (C : Syntaxic_Analysis.Tree.Cursor) is
         N : Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.Element (C);
      
      
      begin
         case N.Node_Type is
         when Syntaxic_Analysis.Node_Var_Decl =>
            declare
               use Syntaxic_Analysis;
               Is_Arg : constant Boolean := (Tree.Element (Tree.Parent (Tree.Parent (C))).Node_Type /= Node_Body_Func);
            begin
               Declare_Ident (Var =>(Symbol_Type => Symbol_Var,
                                     Decl_Line => N.Line,
                                     Is_Referenced => False,
                                     Is_Init => False,
                                     Idx => Nb_Var,
                                     Is_Arg_Var => Is_Arg),
                              Id => N.Var_Key);
            exception
               when e : Compilation_Error =>
                  Error (MSg => Ada.Exceptions.Exception_Message(e),
                         Line => N.Line);
                  raise;
            end;
            declare
               use type Ada.Containers.Count_Type;
            begin
               if Syntaxic_Analysis.Tree.Child_Count (C) = 1 then
                  AST_Analyse_Node (Syntaxic_Analysis.Tree.First_Child (C));
               end if;
            end;
               
         when Syntaxic_Analysis.Node_Var_Ref =>
            declare
               s : Symbol := Search_Ident (Id => N.Ref_Var_Key);
            begin
               if S.Symbol_Type /= Symbol_Var then
                  Error (Msg  => "missing parenthesis in call statement",
                         Line => N.Line);
                  raise Compilation_Error;
               end if;
               N.Var_Stack_Index := S.Idx;
               
               case Syntaxic_Analysis.Tree.Element (Syntaxic_Analysis.Tree.Parent (C)).Node_Type is
               when Syntaxic_Analysis.Node_Op_Assignment =>
                  S.Is_Init := True;
                  Update_Element (Id => N.Ref_Var_Key,
                                  El => S);
               when others =>
                  S.Is_Referenced := True;
                  Update_Element (Id => N.Ref_Var_Key,
                                  El => S);
               end case;
            exception
               when e : Compilation_Error =>
                  Error (MSg => Ada.Exceptions.Exception_Message(e),
                         Line => N.Line);
                  raise;
            end;
            Syntaxic_Analysis.Tree.Replace_Element (Container => T,
                                                    Position  => C,
                                                    New_Item  => N);

            
         when Syntaxic_Analysis.Node_Instruction_Block =>
            Add_Scope;
            Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
            Rem_Scope;
            
         when Syntaxic_Analysis.Node_Body_Func =>
            declare
               Nb_Args : constant Natural := Natural (Syntaxic_Analysis.Tree.Child_Count (Syntaxic_Analysis.Tree.First_Child (C)));
            begin
               begin
                  Declare_Ident (Var =>(Symbol_Type => Symbol_Func,
                                        Decl_Line => N.Line,
                                        Is_Referenced => False,
                                        Nb_Args => Nb_Args),
                                 Id => N.Name_Key);
               exception
                  when e : Compilation_Error =>
                     Error (MSg => Ada.Exceptions.Exception_Message(e),
                            Line => N.Line);
                     raise;
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
               S : Symbol := Search_Ident (Id => N.Ref_Func_Key);
            begin
               if S.Symbol_Type = Symbol_Var then
                  raise Compilation_Error with "Invalid call statment, function not declared";
               elsif S.Nb_Args /= Integer (Syntaxic_Analysis.Tree.Child_Count (C)) then
                  raise Compilation_Error with "Invalid number of argument";
               end if;
               S.Is_Referenced := True;
               Update_Element (Id => N.Ref_Func_Key,
                               El => S);
               
               Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
            exception
               when e : Compilation_Error =>
                  Error (MSg => Ada.Exceptions.Exception_Message(e),
                         Line => N.Line);
                  raise;
            end;
         
         when Syntaxic_Analysis.Node_Address =>
            declare
               N : constant Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.Element (Syntaxic_Analysis.Tree.First_Child (C));
               use type Syntaxic_Analysis.Node_Type_Enum_Type;
            begin
               if Syntaxic_Analysis.Tree.Element (Syntaxic_Analysis.Tree.First_Child (C)).Node_Type /= Syntaxic_Analysis.Node_Var_Ref then
                  Error (Msg => "lvalue required for & operand",
                         Line => N.Line); 
                  raise Compilation_Error;
               end if;
            end;
            Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
               
         when others =>
            Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
         end case;
      end AST_Analyse_Node;
      
   begin
      declare
         M : MAp.Map;
      begin
         M.Insert (Key      => 1,
                   New_Item => (Symbol_Type => Symbol_Func,
                                Decl_Line => 1,
                                Is_Referenced => False,
                                Nb_Args => 1));
         M.Insert (Key      => 2,
                   New_Item => (Symbol_Type => Symbol_Func,
                                Decl_Line => 1,
                                Is_Referenced => False,
                                Nb_Args => 0));
         Var_Vec.Append(M);   
      end;
      AST_Analyse_Node (Syntaxic_Analysis.Tree.First_Child (T.Root));
   end AST_Analyse;
   
   
   procedure Update_Element (Id : Natural; El : Symbol) is
      M : Map.Map;
      Last_Idx : Integer;
   begin
      if Vec.Is_Empty (Var_Vec) then
         raise Program_Error with "Search in a empty stack";
      end if;
      
      Last_Idx := Vec.Last_Index (Var_Vec);
      
      while Last_Idx >= 0 loop
         M := Var_Vec.Element (Last_Idx);
         if M.Contains(Id) then
            Map.Replace (Container => M,
                         Key       => Id,
                         New_Item  => El);
            Var_Vec.Replace_Element (Index    => Last_Idx,
                                     New_Item => M);
            return;
         end if;
         Last_Idx := Last_Idx - 1; 
      end loop;
      raise Program_Error with "this Id is not declare " & Lexical_Analysis.Get_Str_From_Assoc_Table (Id);
   end Update_Element;

   -- declare a variable
   procedure Declare_Ident (Var : Symbol; Id : Natural) is
      M : Map.Map;
   begin
      if Var_Vec.Is_Empty then
         Var_Vec.Append(M);
      end if;
      M := Var_Vec.Last_Element;
      if M.Contains(Id) then
         raise Compilation_Error with "2 declarations of with the same name, conflict : " & Lexical_Analysis.Get_Str_From_Assoc_Table (Id);
      else
         M.Insert (New_Item => Var,
                   Key => Id);
         Var_Vec.Replace_Element(Var_Vec.Last_Index, M);
         Nb_Var := Nb_Var + 1; 
      end if;
   end Declare_Ident;
   
   -- search a Id and return its Symbol
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
      raise Compilation_Error with "this Id is not declare " & Lexical_Analysis.Get_Str_From_Assoc_Table (Id);
   end Search_Ident;
   
   procedure Add_Scope is 
      M : Map.Map;
   begin
      Vec.Append (Var_Vec, M);
   end Add_Scope;
   
   procedure Rem_Scope is
      M : Map.Map;
      procedure Operate (C : Map.Cursor) is
         Id : constant Natural := Map.Key (C);
         S : constant Symbol := Map.Element (C); 
      begin
         if not S.Is_Referenced then
            Warning (Lexical_Analysis.Get_Str_From_Assoc_Table (id) & " is not referenced",
                     Line => S.Decl_Line);
         end if;
         if not S.Is_Init and S.Is_Arg_Var then
            Warning (Lexical_Analysis.Get_Str_From_Assoc_Table (id) & " is not initialise", -- could be false if it is initialise using a pointer
                     Line => S.Decl_Line);
         end if;
        
      end Operate;
   begin
      if Vec.Is_Empty (Var_Vec) then
         raise Program_Error with "removing last scope on a empty stack";
      else
         M := Vec.Last_Element (Var_Vec);
         M.Iterate (Process => Operate'Access);
      end if;
      Vec.Delete_Last (Var_Vec);
   end Rem_Scope;

end Semantic_Analysis;
