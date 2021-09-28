with Syntaxic_Analysis;
package Semantic_Analysis is

   procedure AST_Analyse (T : in out Syntaxic_Analysis.Tree.Tree);
   
   type Symbol is record
      Idx : Natural;
   end record;
   
   -- declare a variable and return its index
   procedure Declare_Var (Var : Symbol);
   
   -- search a variable and return its index
   function Search_Var (Var : Symbol) return Natural;
   
   procedure Add_Scope;
   
   procedure Rem_Scope;

   function Get_Nb_Var return Natural;
   
   


end Semantic_Analysis;
