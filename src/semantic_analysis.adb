with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Error_Log;
use Error_Log;
with Lexical_Analysis;
with Ada.Containers;
with Ada.Assertions;
package body Semantic_Analysis is
   
   Nb_Var : Natural := 0;
   
   function Hash (A : Natural) return Ada.Containers.Hash_Type is (Ada.Containers.Hash_Type (A));
   
   package Scope is new Ada.Containers.Hashed_Maps (Key_Type        => Natural,
                                                    Element_Type    => Symbol,
                                                    Hash            => Hash,
                                                    Equivalent_Keys => "=");
   
   package Scope_Container is new Ada.Containers.Indefinite_Vectors (Index_Type   => Natural,
                                                                     Element_Type => Scope.Map,
                                                                     "="          => Scope."=");
   
   Scopes_Stack : Scope_Container.Vector;
   procedure AST_Analyse (T : in out Syntaxic_Analysis.Tree.Tree) is
      
      procedure AST_Analyse_Node (C : Syntaxic_Analysis.Tree.Cursor) is
         N : Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.Element (C);
      
         Line : constant Positive := N.Line;
         use type Syntaxic_Analysis.Node_Type_Enum_Type;
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
                                     Is_Argument => Is_Arg),
                              Id => N.Var_Key,
                              Line => Line);
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
               s : Symbol := Search_Ident (Id => N.Ref_Var_Key, Line => Line);
            begin
               if S.Symbol_Type /= Symbol_Var then
                  Error (Msg  => "missing parenthesis in call statement",
                         Line => N.Line);
                  raise Input_Error;
               end if;
               N.Var_Stack_OffSett := S.Idx;
               
               case Syntaxic_Analysis.Tree.Element (Syntaxic_Analysis.Tree.Parent (C)).Node_Type is
               when Syntaxic_Analysis.Node_Op_Assignment =>
                  S.Is_Init := True;
                  Update_Element (Id => N.Ref_Var_Key,
                                  El => S,
                                  Line => N.Line);
               when others =>
                  S.Is_Referenced := True;
                  Update_Element (Id => N.Ref_Var_Key,
                                  El => S,
                                  Line => N.Line);
               end case;
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
               Declare_Ident (Var =>(Symbol_Type => Symbol_Func,
                                     Decl_Line => N.Line,
                                     Is_Referenced => False,
                                     Nb_Args => Nb_Args),
                              Id => N.Name_Key,
                              Line => Line);
               
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
               S : Symbol := Search_Ident (Id => N.Ref_Func_Key, Line => Line);
            begin
               if S.Symbol_Type = Symbol_Var then
                  Error_Log.Error (Msg  => "'" & Lexical_Analysis.Get_Str_From_Assoc_Table (N.Ref_Func_Key) & "' undeclared",
                                   Line => Line);
                  raise Input_Error;
               elsif S.Nb_Args /= Integer (Syntaxic_Analysis.Tree.Child_Count (C)) then -- wrong number of argument
                  Error_Log.Error (Msg  => "'" & Lexical_Analysis.Get_Str_From_Assoc_Table (N.Ref_Func_Key) & "' undeclared",
                                   Line => Line);
                  raise Input_Error;
               end if;
               S.Is_Referenced := True;
               Update_Element (Id => N.Ref_Func_Key,
                               El => S,
                               Line => N.Line);
               
               Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
            end;
         
         when Syntaxic_Analysis.Node_Address =>
            declare
               N : constant Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.First_Child_Element (C);
            begin
               if Syntaxic_Analysis.Tree.Element (Syntaxic_Analysis.Tree.First_Child (C)).Node_Type /= Syntaxic_Analysis.Node_Var_Ref and 
               Syntaxic_Analysis.Tree.Element (Syntaxic_Analysis.Tree.First_Child (C)).Node_Type /= Syntaxic_Analysis.Node_Dereference then
                  Error (Msg => "lvalue required for & operand",
                         Line => N.Line); 
                  raise Error_Log.Input_Error;
               end if;
            end;
            Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
               
         when Syntaxic_Analysis.Node_Op_Assignment =>
            declare
               Left : constant Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.First_Child_Element (C);
            begin
               if Left.Node_Type /= Syntaxic_Analysis.Node_Var_Ref and Left.Node_Type /= Syntaxic_Analysis.Node_Dereference then
                  Error_Log.Error (Msg  => "lvalue required as left operand of assignment",
                                   Line => N.Line);
                  raise Error_Log.Input_Error;
               end if;
            end;
            Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
            
         when others =>
            Syntaxic_Analysis.Tree.Iterate_Children (Parent => C, Process => AST_Analyse_Node'Access);
         end case;
      end AST_Analyse_Node;
      
   begin
      AST_Analyse_Node (Syntaxic_Analysis.Tree.First_Child (T.Root));
   end AST_Analyse;
   
   
   procedure Update_Element (Id : Natural; El : Symbol; Line : Positive) is
      M : Scope.Map;
      Last_Idx : Integer;
   begin
      Ada.Assertions.Assert (not Scopes_Stack.Is_Empty, "Empty Scopes Stack");
      
      Last_Idx := Scope_Container.Last_Index (Scopes_Stack);
      
      while Last_Idx >= 0 loop
         M := Scopes_Stack.Element (Last_Idx);
         if M.Contains(Id) then
            Scope.Replace (Container => M,
                           Key       => Id,
                           New_Item  => El);
            Scopes_Stack.Replace_Element (Index    => Last_Idx,
                                          New_Item => M);
            return;
         end if;
         Last_Idx := Last_Idx - 1; 
      end loop;
      Error_Log.Error (Msg  => "this Id is not declare " & Lexical_Analysis.Get_Str_From_Assoc_Table (Id),
                       Line => Line);
      raise Error_Log.Input_Error;
   end Update_Element;

   -- declare a variable
   procedure Declare_Ident (Var : Symbol; Id : Natural; Line : Positive) is
      M : Scope.Map;
   begin
      Ada.Assertions.Assert (not Scopes_Stack.Is_Empty, "Empty Scopes Stack");
      M := Scopes_Stack.Last_Element;
      if M.Contains(Id) then
         Error_Log.Error (Msg  => "redifinition of " & Lexical_Analysis.Get_Str_From_Assoc_Table (Id),
                          Line => Line);
         raise Input_Error;
      else
         M.Insert (New_Item => Var,
                   Key => Id);
         Scopes_Stack.Replace_Element(Scopes_Stack.Last_Index, M);
         Nb_Var := Nb_Var + 1; 
      end if;
   end Declare_Ident;
   
   -- search a Id and return its Symbol
   function Search_Ident (Id : Natural; Line : Positive) return Symbol is
      M : Scope.Map;
      Last_Idx : Integer;
   begin
      Ada.Assertions.Assert (not Scopes_Stack.Is_Empty, "Empty Scopes Stack");
      Last_Idx := Scope_Container.Last_Index (Scopes_Stack);
      
      while Last_Idx >= 0 loop
         M := Scopes_Stack.Element (Last_Idx);
         if M.Contains(Id) then
            return Scope.Element (M, Id);
         end if;
         Last_Idx := Last_Idx - 1; 
      end loop;
      Error_Log.Error (Msg  => "'" & Lexical_Analysis.Get_Str_From_Assoc_Table (Id) & "' undeclared",
                       Line => Line);
      raise Input_Error;
   end Search_Ident;
   
   procedure Add_Scope is 
      M : Scope.Map;
   begin
      Scope_Container.Append (Scopes_Stack, M);
   end Add_Scope;
   
   procedure Rem_Scope is
      M : Scope.Map;
      procedure Operate (C : Scope.Cursor) is
         Id : constant Natural := Scope.Key (C);
         S : constant Symbol := Scope.Element (C); 
      begin
         if not S.Is_Referenced then
            Warning (Lexical_Analysis.Get_Str_From_Assoc_Table (id) & " is not referenced",
                     Line => S.Decl_Line);
         end if;
         if not S.Is_Init and S.Is_Argument then
            Warning (Lexical_Analysis.Get_Str_From_Assoc_Table (id) & " is not initialize", -- could be false if it is initialize using a pointer
                     Line => S.Decl_Line);
         end if;
        
      end Operate;
   begin
      Ada.Assertions.Assert (not Scopes_Stack.Is_Empty, "Empty Scopes Stack");

      M := Scope_Container.Last_Element (Scopes_Stack);
      M.Iterate (Process => Operate'Access);
      Scope_Container.Delete_Last (Scopes_Stack);
   end Rem_Scope;

   
begin
   declare
      M : Scope.Map;
   begin
      -- putchar
      M.Insert (Key      => 1,
                New_Item => (Symbol_Type => Symbol_Func,
                             Decl_Line => 1,
                             Is_Referenced => True,
                             Nb_Args => 1));
      -- getchar
      M.Insert (Key      => 2,
                New_Item => (Symbol_Type => Symbol_Func,
                             Decl_Line => 1,
                             Is_Referenced => True,
                             Nb_Args => 0));
      -- exit
      M.Insert (Key      => 3,
                New_Item => (Symbol_Type => Symbol_Func,
                             Decl_Line => 1,
                             Is_Referenced => True,
                             Nb_Args => 0));
      Scopes_Stack.Append(M);   
   end;
end Semantic_Analysis;
