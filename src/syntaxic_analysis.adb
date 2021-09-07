with Lexical_Analysis;
with Token;
with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Syntaxic_Analysis is

   function Same_Node (L,R : Node) return Boolean is
      Ok : Boolean := L.Node_Type = R.Node_Type and L.Line = R.Line and L.Has_Value = R.Has_Value;
   begin
      if ok and L.Has_Value then
         ok := L.Value = R.Value;
      end if;
      return ok;
   end Same_Node;


   function G return Tree.Tree is
   begin
      return E;
   end G;

   function E return Tree.Tree is
   begin
      return P;
   end E;

   function P return Tree.Tree is
   begin
      if Lexical_Analysis.Check_Token(Token.Tok_Plus) then
         return P;
      elsif Lexical_Analysis.Check_Token(Token.Tok_Minus) then
         declare
            T  : Tree.Tree := P;
            NT : Tree.Tree;
            Pos : Tree.Cursor;
            unused : Tree.Cursor := Tree.First_Child (T.Root);
         begin
            NT.Insert_Child (Parent   => NT.Root,
                             Before   => Tree.No_Element,
                             New_Item => (Has_Value => False,
                                          Node_Type => Node_Minus_U,
                                          Line => Lexical_Analysis.Get_Current_Token.Line),
                             Position => Pos);

            Tree.Splice_Subtree (Target => NT,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => T,
                                 Position => unused);

            return NT;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_Exclamation_Mark) then
         declare
            T  : Tree.Tree := P;
            NT : Tree.Tree;
            Pos : Tree.Cursor;
            unused : Tree.Cursor := Tree.First_Child (T.Root);
         begin
            NT.Insert_Child (Parent   => NT.Root,
                             Before   => Tree.No_Element,
                             New_Item => (Has_Value => False,
                                          Node_Type => Node_Not,
                                          Line => Lexical_Analysis.Get_Current_Token.Line),
                             Position => Pos);

            Tree.Splice_Subtree (Target => NT,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => T,
                                 Position => unused);


            return NT;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_Asterisk) then
         declare
            T  : Tree.Tree := P;
            NT : Tree.Tree;
            Pos : Tree.Cursor;
            unused : Tree.Cursor := Tree.First_Child (T.Root);
         begin
            NT.Insert_Child (Parent   => NT.Root,
                             Before   => Tree.No_Element,
                             New_Item => (Has_Value => False,
                                          Node_Type => Node_Dereference,
                                          Line => Lexical_Analysis.Get_Current_Token.Line),
                             Position => Pos);

          Tree.Splice_Subtree (Target => NT,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => T,
                                 Position => unused);

            return NT;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_Ampersand) then
         declare
            T  : Tree.Tree := P;
            NT : Tree.Tree;
            Pos : Tree.Cursor;
            unused : Tree.Cursor := Tree.First_Child (T.Root);
         begin
            NT.Insert_Child (Parent   => NT.Root,
                             Before   => Tree.No_Element,
                             New_Item => (Has_Value => False,
                                          Node_Type => Node_Address,
                                          Line => Lexical_Analysis.Get_Current_Token.Line),
                             Position => Pos);

            Tree.Splice_Subtree (Target => NT,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => T,
                                 Position => unused);

            return NT;
         end;
      else
         return S;

      end if;

   end P;

   function A return Tree.Tree is
   begin
      if Lexical_Analysis.Check_Token (Token.Tok_Const) then
         declare
            T : Tree.Tree;
         begin
            T.Insert_Child (Parent   => T.Root,
                            Before   => Tree.No_Element,
                            New_Item => (Has_Value => True,
                                         Node_Type => Node_Constant,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Value => Lexical_Analysis.Get_Current_Token.Value));
            return T;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_Left_Parenthesis) then
         declare
            T : constant Tree.Tree := E;
         begin
            Lexical_Analysis.Accept_Token (Token.Tok_Right_Parenthesis);
            return T;
         end;
      else
         Lexical_Analysis.Error (msg  => "An atomic was expected here",
                                 Line => Lexical_Analysis.Get_Current_Token.Line);
         raise Constraint_Error;
      end if;
   end A;

   function S return Tree.Tree is
   begin
      return A;
   end S;


   function Debug_Print (N : Node) return string is
   begin
      if not N.Has_Value then
         return "(" & N.Node_Type'Image & ", l:" & N.Line'Image & ")";
      else
         return "(" & N.Node_Type'Image & ", val:" & N.Value'Image & ", l:" & N.Line'Image & ")";
      end if;
   end Debug_Print;

   procedure Debug_Print_Tree (T : Tree.Tree) is
      Debug_File_Tree : Ada.Text_IO.File_Type;

      procedure Print_Tree (C : Tree.Cursor) is
         use Ada.Strings.Fixed;
         N : constant Node := Tree.Element (C);
         P : constant Natural := Natural (Tree.Depth (C))-2;
         str : constant string := P * "--";
      begin
            Ada.Text_IO.Put (Debug_File_Tree, str);
            Ada.Text_IO.Put_Line (Debug_File_Tree, Debug_Print (N));
      end Print_Tree;

   begin
      Ada.Text_IO.Create (File => Debug_File_Tree,
                          Mode => Ada.Text_IO.Out_File,
                          Name => "tree.txt");
      Tree.Iterate (T, Print_Tree'Access);
      Ada.Text_IO.Close (Debug_File_Tree);
   end Debug_Print_Tree;

end Syntaxic_Analysis;

