with Lexical_Analysis;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Tree_Graphviz;

package body Syntaxic_Analysis is

   Op_Table : array (Operation_Token) of Operation_Record_Type;

   function Set_Discriminant (Node_Type : Node_Type_Enum_Type) return Node_Variant_Type is
      pragma Warnings(off, "variable ""Result"" is read but never assigned");
   begin
      return Result : Node_Variant_Type (Node_Type => Node_Type);
   end Set_Discriminant;

   procedure Init is
      use Token;
   begin
      Op_Table (Tok_Assignment) := (Left_Priority => 1,
                                    Right_Priority => 1,
                                    Node => Node_Op_Assignment);

      Op_Table (Tok_Or_Boolean) := (Left_Priority => 2,
                                    Right_Priority => 3,
                                    Node => Node_Op_Or_Boolean);

      Op_Table (Tok_And_Boolean) := (Left_Priority => 3,
                                     Right_Priority => 4,
                                     Node => Node_Op_And_Boolean);

      Op_Table (Tok_Equal_Comparaison) := (Left_Priority => 4,
                                           Right_Priority => 5,
                                           Node => Node_Op_Equal_Comparaison);
      Op_Table (Tok_Difference_Comparaison) := (Left_Priority => 4,
                                                Right_Priority => 5,
                                                Node => Node_Op_Difference_Comparaison);

      Op_Table (Tok_Greater_than) := (Left_Priority => 5,
                                      Right_Priority => 6,
                                      Node => Node_Op_Greater_than);
      Op_Table (Tok_Greater_Or_Equal_Than) := (Left_Priority => 5,
                                               Right_Priority => 6,
                                               Node => Node_Op_Greater_Or_Equal_Than);
      Op_Table (Tok_Less_Than) := (Left_Priority => 5,
                                   Right_Priority => 6,
                                   Node => Node_Op_Less_Than);
      Op_Table (Tok_Less_Or_Equal_Than) := (Left_Priority => 5,
                                            Right_Priority => 6,
                                            Node => Node_Op_Less_Or_Equal_Than);

      Op_Table (Tok_Plus) := (Left_Priority => 6,
                              Right_Priority => 7,
                              Node => Node_Op_Add);
      Op_Table (Tok_Minus) := (Left_Priority => 6,
                               Right_Priority => 7,
                               Node => Node_Op_Sub);

      Op_Table (Tok_Asterisk) := (Left_Priority => 7,
                                  Right_Priority => 8,
                                  Node => Node_Op_Mult);
      Op_Table (Tok_Slash) := (Left_Priority => 7,
                               Right_Priority => 8,
                               Node => Node_Op_Division);
      Op_Table (Tok_Percent) := (Left_Priority => 7,
                                 Right_Priority => 8,
                                 Node => Node_Op_Modulo);
   end Init;

   function G return Tree.Tree is
   begin
      return I;
   end G;

   function E (Min_Priority : Priority := Priority (0)) return Tree.Tree is
   begin
      declare
         N : Tree.Tree := P;
      begin

         while (not Lexical_Analysis.EOF) loop
            declare
               Next : constant Token.Token_Record_Type := Lexical_Analysis.Get_Next_Token;
            begin

               if not (Next.Token_Type in Operation_Token) then
                  return N;
               end if;

               declare
                  Op : constant Operation_Record_Type := Op_Table (Next.Token_Type);
               begin
                  if Op.Left_Priority < Min_Priority then
                     return N;
                  end if;
                  Lexical_Analysis.Advance_Token;
                  declare
                     A : Tree.Tree := E(Op.Right_Priority);
                     New_N : Tree.Tree;
                     Pos : Tree.Cursor;
                     unused : Tree.Cursor := Tree.First_Child (N.Root);
                     unused2 : Tree.Cursor := Tree.First_Child (A.Root);
                     New_Item : Node_Variant_Type := Set_Discriminant (Op.Node);
                  begin
                     New_Item.Line := Next.Line;
                     New_N.Insert_Child (Parent   => New_N.Root,
                                         Before   => Tree.No_Element,
                                         New_Item => New_Item,
                                         Position    => Pos);

                     Tree.Splice_Subtree (Target => New_N,
                                          Parent   => Pos,
                                          Before   => Tree.No_Element,
                                          Source   => N,
                                          Position => unused);

                     Tree.Splice_Subtree (Target => New_N,
                                          Parent   => Pos,
                                          Before   => Tree.No_Element,
                                          Source   => A,
                                          Position => unused2);
                     N := New_N;
                  end;
               end;
            end;
         end loop;

         return N;
      end;
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
                             New_Item => (Node_Type => Node_Minus_U,
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
                             New_Item => (Node_Type => Node_Not,
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
                             New_Item => (Node_Type => Node_Dereference,
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
                             New_Item => (Node_Type => Node_Address,
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
                            New_Item => (Node_Type => Node_Constant,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Value => Lexical_Analysis.Get_Current_Token.Value));
            return T;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_Id) then
         declare
            T : Tree.Tree;
            begin
               T.Insert_Child (Parent   => T.Root,
                               Before   => Tree.No_Element,
                               New_Item => (Node_Type => Node_Var_Ref,
                                            Line => Lexical_Analysis.Get_Current_Token.Line,
                                            Ref_Var_Key => Lexical_Analysis.Get_Current_Token.Value,
                                            Var_Stack_Index => 0));
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



   function I return Tree.Tree is
   begin
      if Lexical_Analysis.Check_Token (Token.Tok_Left_Curly_Bracket) then
         declare
            T : Tree.Tree;
            Pos : Tree.Cursor;
         begin
            T.Insert_Child (Parent   => T.Root,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Instruction_Block,
                                         Line => Lexical_Analysis.Get_Current_Token.Line),
                            Position => Pos);
            while not Lexical_Analysis.Check_Token (Token.Tok_Right_Curly_Bracket) loop

               declare
                  N : Tree.Tree := I;
                  unused : Tree.Cursor := Tree.First_Child (N.Root);
               begin

                  Tree.Splice_Subtree (Target   => T,
                                       Parent   => Pos,
                                       Before   => Tree.No_Element,
                                       Source   => N,
                                       Position => unused);
               end;
            end loop;
            return T;
         end;
      elsif Lexical_Analysis.Check_Token (Token_Type => Token.Tok_Debug) then
         declare
            T : Tree.Tree := E;
            Pos : Tree.Cursor;
            NT : Tree.Tree;
            unused : Tree.Cursor := Tree.First_Child (T.Root);
         begin
            NT.Insert_Child (Parent   => NT.Root,
                             Before   => Tree.No_Element,
                             New_Item => (Node_Type => Node_Debug,
                                          Line => Lexical_Analysis.Get_Current_Token.Line),
                             Position => Pos);

            Lexical_Analysis.Accept_Token (Token.Tok_Semi_Colon);

            Tree.Splice_Subtree (Target   => NT,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => T,
                                 Position => unused);
            return NT;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_Int) then
         declare
            N : Tree.Tree;
            Pos : Tree.Cursor;
            unused : Tree.Cursor;
         begin
            N.Insert_Child (Parent   => N.Root,
                               Before   => Tree.No_Element,
                               New_Item => (Node_Type => Node_Seq,
                                            Line => Lexical_Analysis.Get_Current_Token.Line),
                               Position => Pos);

            Lexical_Analysis.Accept_Token (Token.Tok_Id);

            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Var_Decl,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Var_Key => Lexical_Analysis.Get_Current_Token.Value));

            while not Lexical_Analysis.Check_Token (Token.Tok_Semi_Colon) loop
               Lexical_Analysis.Accept_Token (Token.Tok_Comma);
               Lexical_Analysis.Accept_Token (Token.Tok_Id);
               N.Insert_Child (Parent   => Pos,
                               Before   => Tree.No_Element,
                               New_Item => (Node_Type => Node_Var_Decl,
                                            Line => Lexical_Analysis.Get_Current_Token.Line,
                                            Var_Key => Lexical_Analysis.Get_Current_Token.Value));
            end loop;
            return N;
         end;
      else
         declare
            T : Tree.Tree := E;
            NT : Tree.Tree;
            unused : Tree.Cursor := Tree.First_Child (T.Root);
            Pos : Tree.Cursor;
         begin
            NT.Insert_Child (Parent   => NT.Root,
                             Before   => Tree.No_Element,
                             New_Item => (Node_Type => Node_Drop,
                                          Line => Lexical_Analysis.Get_Current_Token.Line),
                             Position => Pos);

            Lexical_Analysis.Accept_Token (Token.Tok_Semi_Colon);

            Tree.Splice_Subtree (Target   => NT,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => T,
                                 Position => unused);
            return NT;
         end;
      end if;
   end I;

   function Same_Node (L,R : Node_Variant_Type) return Boolean is
      Ok : Boolean := L.Node_Type = R.Node_Type and L.Line = R.Line;
   begin
      if ok then
         case L.Node_Type is
         when Node_Constant =>
            Ok := L.Value = R.Value;
         when Node_Var_Decl =>
            Ok := L.Var_Key = R.Var_Key;
         when Node_Var_Ref =>
            Ok := L.Ref_Var_Key = R.Ref_Var_Key and L.Var_Stack_Index = R.Var_Stack_Index;
         when others => null;
         end case;
      end if;
      return ok;
   end Same_Node;

   function Debug_Print (N : Node_Variant_Type) return string is
   begin
      case N.Node_Type is
         when Node_Constant =>
            return "(" & N.Node_Type'Image & ", Val:" & N.Value'Image & ", l:" & N.Line'Image & ")";
         when Node_Var_Decl =>
            return "(" & N.Node_Type'Image & ", Var_Key:" & N.Var_Key'Image & ", l:" & N.Line'Image & ")";
         when Node_Var_Ref =>
            return "(" & N.Node_Type'Image & ", Ref_Var_Key:" & N.Ref_Var_Key'Image & ", Var_Stack_Idx:" & N.Var_Stack_Index'Image & " l:" & N.Line'Image & ")";
         when others =>
            return "(" & N.Node_Type'Image & ", l:" & N.Line'Image & ")";
      end case;
   end Debug_Print;

   procedure Debug_Print_Tree (T : Tree.Tree) is
      Debug_File_Tree : Ada.Text_IO.File_Type;

      procedure Print_Tree (C : Tree.Cursor) is
         use Ada.Strings.Fixed;
         N : constant Node_Variant_Type := Tree.Element (C);
         P : constant Natural := Natural (Tree.Depth (C))-2;
         str : constant string := P * "----";
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


   procedure Debug_Print_Tree_Graphviz (T : Tree.Tree) is
      function Get_label (C : Tree.Cursor) return string is
         N : constant Node_Variant_Type := Tree.Element (C);
         Img : constant String := N.Node_Type'Image;
         Index : constant Integer := Ada.Strings.Fixed.Index (Img, "_");
         Node_Img : constant String := Img (Index + 1..Img'Last);
      begin
         case N.Node_Type is
         when Node_Constant =>
            return Node_Img & "(Val:" & N.Value'Image &")";
         when Node_Var_Decl =>
            return Node_Img & "(Var_Key:" & N.Var_Key'Image & ")";
         when Node_Var_Ref =>
            return Node_Img & "(Ref_Var_Key:" & N.Ref_Var_Key'Image & ", Var_Stack_Idx:" & N.Var_Stack_Index'Image & ")";
         when others =>
            return Node_Img;
         end case;
      end Get_label;
      function Get_arrow (C : Tree.Cursor) return string is
         pragma Unreferenced(C);
      begin
         return "";
      end Get_arrow;
      package Graphviz is new Tree_Graphviz (Cursor          => Tree.Cursor,
                                             Depth           => Tree.Depth,
                                             Get_Node_Label  => Get_label,
                                             Get_Arrow_Label => Get_arrow);
      use Ada.Text_IO;
      F : File_Type;
   begin
      Create (File => F,
              Mode => Out_File,
              Name => "tree.gv");
      Put_Line (F, "diGraph Tree {");
      For C in T.Iterate loop
         Graphviz.Put (F, C);
      end loop;
      Put_Line (F, "}");
      close (F);
   end Debug_Print_Tree_Graphviz;


end Syntaxic_Analysis;

