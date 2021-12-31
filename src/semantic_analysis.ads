with Syntaxic_Analysis;
package Semantic_Analysis is

   procedure AST_Analyse (T : in out Syntaxic_Analysis.Tree.Tree);
   
   type Symbol_Type_Enum_Type is (Symbol_Var, Symbol_Func);
   
   type Symbol (Symbol_Type : Symbol_Type_Enum_Type := Symbol_Var) is record
      Decl_Line : Positive;
      Is_Referenced : Boolean;
      case Symbol_Type is
         when Symbol_Func =>
            Nb_Args : Natural;
         when Symbol_Var => 
            Idx : Natural;
            Is_Init : Boolean;
            Is_Argument : Boolean;
      end case;
   end record;
   
   -- declare a variable and return its index
   procedure Declare_Ident (Var : Symbol; Id : Natural; Line : Positive);
   
   -- search a variable and return its index
   function Search_Ident (Id : Natural; Line : Positive) return Symbol;
   
   procedure Update_Element (Id : Natural; El : Symbol);
   
   procedure Add_Scope;
   
   procedure Rem_Scope;

   function Get_Nb_Var return Natural;
   
   


end Semantic_Analysis;
