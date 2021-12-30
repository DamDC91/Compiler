with Lexical_Analysis;
with Error_Log;
use Error_Log;
package body Syntaxic_Analysis is

   Op_Table : array (Operation_Token) of Operation_Record_Type;

   Condition_Count : Positive := 1;
   Loop_Count : Positive := 1;

   function Set_Discriminant (Node_Type : Node_Type_Enum_Type) return Node_Variant_Type is
      pragma Warnings(off, "variable ""Result"" is read but never assigned");
   begin
      return Result : Node_Variant_Type (Node_Type => Node_Type);
   end Set_Discriminant;

   procedure Init is
   begin
      null;
   end Init;

   function G return Tree.Tree is
      N : Tree.Tree;
      Tmp : Tree.Tree;
      pos : Tree.Cursor;
   begin
      N.Insert_Child (Parent   => N.Root,
                      Before   => Tree.No_Element,
                      New_Item => (Node_Type => Node_Seq,
                                   Line => 1));

      while not Lexical_Analysis.EOF loop
         Tmp := F;
         Pos := Tree.First_Child (Tmp.Root);
         Tree.Splice_Subtree (Target   => N,
                              Parent   => Tree.First_Child (N.Root),
                              Before   => Tree.No_Element,
                              Source   => Tmp,
                              Position => Pos);
      end loop;
      if Error_Log.Get_Debug_On then
         Debug_Print_Tree (N);
         Debug_Print_Tree_Graphviz (N);
         Lexical_Analysis.Close_Debug;
      end if;
      return N;
   exception
      when others =>
         if Error_Log.Get_Debug_On then
            Debug_Print_Tree (N);
            Debug_Print_Tree_Graphviz (N);
            Lexical_Analysis.Close_Debug;
         end if;
         raise;
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
            Id : constant Token.Token_Record_Type := Lexical_Analysis.Get_Current_Token;
         begin
            if Lexical_Analysis.Check_Token (Token.Tok_Left_Parenthesis) then
               declare
                  N : Tree.Tree;
                  Pos : Tree.Cursor;
               begin
                  N.Insert_Child (Parent   => N.Root,
                                  Before   => Tree.No_Element,
                                  New_Item => (Node_Type => Node_Call,
                                               Line => ID.Line,
                                               Ref_Func_Key => ID.Value),
                                  Position    => Pos);
                  while not Lexical_Analysis.Check_Token (Token.Tok_Right_Parenthesis) loop
                     declare
                        E1 : Tree.Tree := E;
                        Pos_Sub : Tree.Cursor := Tree.First_Child (E1.Root);
                        unused : constant Boolean := Lexical_Analysis.Check_Token (Token.Tok_Comma);
                     begin
                        Tree.Splice_Subtree (Target   => N,
                                             Parent   => Pos,
                                             Before   => Tree.No_Element,
                                             Source   => E1,
                                             Position => Pos_sub);
                     end;
                  end loop;
                  return N;
               end;
            else
               declare
                  T : Tree.Tree;
               begin
                  T.Insert_Child (Parent   => T.Root,
                                  Before   => Tree.No_Element,
                                  New_Item => (Node_Type => Node_Var_Ref,
                                               Line => Id.Line,
                                               Ref_Var_Key => Id.Value,
                                               Var_Stack_Index => 0));
                  return T;
               end;
            end if;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_Left_Parenthesis) then
         declare
            T : constant Tree.Tree := E;
         begin
            Lexical_Analysis.Accept_Token (Token.Tok_Right_Parenthesis);
            return T;
         end;
      else
         Error (msg  => "An atomic was expected here, found " & Lexical_Analysis.Get_Current_Token.Token_Type'Image,
                Line => Lexical_Analysis.Get_Current_Token.Line);
         raise Compilation_Error;
      end if;
   end A;

   function S return Tree.Tree is
   begin
      declare
         Res : Tree.Tree := A;
      begin
         while Lexical_Analysis.Check_Token (Token.Tok_Left_Bracket) loop
            declare
               E1 : Tree.Tree := E;
               Tmp : Tree.Tree := Res.Copy;
               Pos : Tree.Cursor;
               Pos_Sub : Tree.Cursor;
            begin
               Tree.Clear(Res);

               Res.Insert_Child (Parent    => Res.Root,
                                 Before    => Tree.No_Element,
                                 New_Item  => (Node_Type => Node_Dereference,
                                               Line => Lexical_Analysis.Get_Current_Token.Line),
                                 Position  => Pos);
               Res.Insert_Child (Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 New_Item => (Node_Type => Node_Op_Add,
                                              Line => Lexical_Analysis.Get_Current_Token.Line),
                                 Position    => Pos_Sub);
               Pos := Pos_Sub;

               Pos_Sub := Tree.First_Child (Tmp.Root);

               Tree.Splice_Subtree (Target   => Res,
                                    Parent   => Pos,
                                    Before   => Tree.No_Element,
                                    Source   => Tmp,
                                    Position => Pos_Sub);

               Pos_Sub := tree.First_Child (E1.Root);
               Tree.Splice_Subtree (Target   => Res,
                                    Parent   => Pos,
                                    Before   => Tree.No_Element,
                                    Source   => E1,
                                    Position => Pos_Sub);
               Lexical_Analysis.Accept_Token (Token.Tok_Right_Bracket);
            end;
         end loop;
         return Res;
      end;
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
      elsif Lexical_Analysis.Check_Token (Token.Tok_Int) then
         declare
            N : Tree.Tree;
            Pos : Tree.Cursor;
            unused : Tree.Cursor;
            Id : constant Token.Token_Record_Type := Lexical_Analysis.Get_Next_Token;
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

            if Lexical_Analysis.Check_Token (Token.Tok_Assignment) then
               declare
                  E1 : Tree.Tree := E;
                  Pos2 : Tree.Cursor := Tree.First_Child (Pos);
                  Pos_Sub : Tree.Cursor;
               begin
                  N.Insert_Child (Parent   => Pos2,
                                  Before   => Tree.No_Element,
                                  New_Item => (Node_Type => Node_Op_Assignment,
                                               Line => Lexical_Analysis.Get_Current_Token.Line),
                                  Position    => Pos_Sub);
                  Pos2 := Pos_Sub;

                  N.Insert_Child (Parent   => Pos2,
                                  Before   => Tree.No_Element,
                                  New_Item => (Node_Type => Node_Var_Ref,
                                               Line => Id.Line,
                                               Ref_Var_Key => Id.Value,
                                               Var_Stack_Index => 0));

                  Pos_Sub := Tree.First_Child (E1.Root);
                  Tree.Splice_Subtree (Target   => N,
                                       Parent   => Pos2,
                                       Before   => Tree.No_Element,
                                       Source   => E1,
                                       Position => Pos_Sub);
               end;
            end if;

            while not Lexical_Analysis.Check_Token (Token.Tok_Semi_Colon) loop
               Lexical_Analysis.Accept_Token (Token.Tok_Comma);
               Lexical_Analysis.Accept_Token (Token.Tok_Id);
               declare
                  Id : constant Token.Token_Record_Type := Lexical_Analysis.Get_Current_Token;
                  Pos2 : Tree.Cursor;
               begin
                  N.Insert_Child (Parent   => Pos,
                                  Before   => Tree.No_Element,
                                  New_Item => (Node_Type => Node_Var_Decl,
                                               Line => Lexical_Analysis.Get_Current_Token.Line,
                                               Var_Key => Lexical_Analysis.Get_Current_Token.Value),
                                  Position => Pos2);

                  if Lexical_Analysis.Check_Token (Token.Tok_Assignment) then
                     declare
                        E1 : Tree.Tree := E;
                        Pos_Sub : Tree.Cursor;
                     begin
                        N.Insert_Child (Parent   => Pos2,
                                        Before   => Tree.No_Element,
                                        New_Item => (Node_Type => Node_Op_Assignment,
                                                     Line => Lexical_Analysis.Get_Current_Token.Line),
                                        Position    => Pos_Sub);
                        Pos2 := Pos_Sub;
                        N.Insert_Child (Parent   => Pos2,
                                        Before   => Tree.No_Element,
                                        New_Item => (Node_Type => Node_Var_Ref,
                                                     Line => Id.Line,
                                                     Ref_Var_Key => Id.Value,
                                                     Var_Stack_Index => 0));

                        Pos_Sub := Tree.First_Child (E1.Root);
                        Tree.Splice_Subtree (Target   => N,
                                             Parent   => Pos2,
                                             Before   => Tree.No_Element,
                                             Source   => E1,
                                             Position => Pos_Sub);
                     end;
                  end if;
               end;
            end loop;
            return N;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_If) then
         Lexical_Analysis.Accept_Token (Token.Tok_Left_Parenthesis);
         declare
            Cond_Node : Tree.Tree;
            E_Node : Tree.Tree := E;
            I1_Node : Tree.Tree;
            Pos : Tree.Cursor;
            Pos_Sub : Tree.Cursor;
         begin
            Cond_Node.Insert_Child (Parent => Cond_Node.Root,
                                    Before => Tree.No_Element,
                                    New_Item => (Node_Type => Node_Cond,
                                                 Line => Lexical_Analysis.Get_Current_Token.Line,
                                                 Cond_Count => Condition_Count),
                                    Position => Pos);

            Condition_Count := Condition_Count + 1;

            Lexical_Analysis.Accept_Token(Token.Tok_Right_Parenthesis);
            I1_Node := I;

            Pos_Sub := Tree.First_Child (E_Node.Root);
            Tree.Splice_Subtree (Target   => Cond_Node,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => E_Node,
                                 Position => Pos_Sub);

            Pos_Sub := Tree.First_Child (I1_Node.Root);
            Tree.Splice_Subtree (Target   => Cond_Node,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => I1_Node,
                                 Position => Pos_Sub);

            if Lexical_Analysis.Check_Token(Token.Tok_Else) then

               declare
                  I2_Node : Tree.Tree := I;
               begin
                  Pos_Sub := Tree.First_Child (I2_Node.Root);
                  Tree.Splice_Subtree (Target   => Cond_Node,
                                       Parent   => Pos,
                                       Before   => Tree.No_Element,
                                       Source   => I2_Node,
                                       Position => Pos_Sub);
               end;
            end if;
            return Cond_Node;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_While) then
         Lexical_Analysis.Accept_Token (Token.Tok_Left_Parenthesis);
         declare
            Loop_Node : Tree.Tree;
            Cond_Node : Tree.Tree;
            E_Node : Tree.Tree;
            I_Node : Tree.Tree;
            Pos : Tree.Cursor;
            Pos_Sub : Tree.Cursor;
            Current_Loop_Nb : constant Positive := Loop_Count;
         begin
            Loop_Node.Insert_Child (Parent   => Loop_Node.Root,
                                    Before   => Tree.No_Element,
                                    New_Item => (Node_Type => Node_Loop,
                                                 Line => Lexical_Analysis.Get_Current_Token.Line,
                                                 Loop_Count => Loop_Count),
                                    Position    => Pos);
            Loop_Count := Loop_Count + 1;

            E_Node := E;
            Lexical_Analysis.Accept_Token (Token.Tok_Right_Parenthesis);
            I_Node := I;

            Cond_Node.Insert_Child (Parent   => Cond_Node.Root,
                                    Before   => Tree.No_Element,
                                    New_Item => (Node_Type => Node_Label,
                                                 Line => Lexical_Analysis.Get_Current_Token.Line,
                                                 Loop_Count => Current_Loop_Nb),
                                    Position    => Pos);

            Cond_Node.Insert_Child (Parent   => Pos,
                                    Before   => Tree.No_Element,
                                    New_Item => (Node_Type => Node_Cond,
                                                 Line => Lexical_Analysis.Get_Current_Token.Line,
                                                 Cond_Count => Condition_Count),
                                    Position    => Pos_Sub);
            Pos := Pos_Sub;

            Pos_Sub := Tree.First_Child (E_Node.Root);
            Tree.Splice_Subtree (Target   => Cond_Node,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => E_Node,
                                 Position => Pos_Sub);

            Pos_Sub := Tree.First_Child (I_Node.Root);
            Tree.Splice_Subtree (Target   => Cond_Node,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => I_Node,
                                 Position => Pos_Sub);

            Cond_Node.Insert_Child (Parent => Pos,
                                    Before => Tree.No_Element,
                                    New_Item => (Node_Type => Node_Break,
                                                 Line => Lexical_Analysis.Get_Current_Token.Line,
                                                 Loop_Count => Current_Loop_Nb));



            Pos_Sub := Tree.First_Child (Cond_Node.Root);
            Tree.Splice_Subtree (Target   => Loop_Node,
                                 Parent   => Tree.First_Child (Loop_Node.Root),
                                 Before   => Tree.No_Element,
                                 Source   => Cond_Node,
                                 Position => Pos_Sub);

            Condition_Count := Condition_Count + 1;

            return Loop_Node;
         end;

      elsif Lexical_Analysis.Check_Token (Token.Tok_For) then
         declare
            N : Tree.Tree;
            E1 : Tree.Tree;
            E2 : Tree.Tree;
            E3 : Tree.Tree;
            I1 : Tree.Tree;
            Pos: Tree.Cursor;
            Pos_Sub : Tree.Cursor;
            Current_Loop_Nb : constant Positive := Loop_Count;
         begin


            Lexical_Analysis.Accept_Token (Token.Tok_Left_Parenthesis);
            E1 := E;

            N.Insert_Child (Parent   => N.Root,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Seq,
                                         Line => Lexical_Analysis.Get_Current_Token.Line),
                            Position    => Pos);

            Tree.Insert_Child (Container => N,
                               Parent    => Pos,
                               Before    => Tree.No_Element,
                               New_Item  => (Node_Type => Node_Drop,
                                             Line => Lexical_Analysis.Get_Current_Token.Line),
                               Position     => Pos_Sub);

            Pos := Pos_Sub;
            Pos_Sub := Tree.First_Child (E1.Root);
            Tree.Splice_Subtree (Target   => N,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => E1,
                                 Position => Pos_Sub);

            Pos := Tree.First_Child (N.Root);
            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Loop,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Loop_Count => Loop_Count),
                            Position    => Pos_Sub);
            Pos := Pos_Sub;

            Loop_Count := Loop_Count + 1;

            Lexical_Analysis.Accept_Token (Token.Tok_Semi_Colon);
            E2 := E;
            Lexical_Analysis.Accept_Token (Token.Tok_Semi_Colon);
            E3 := E;
            Lexical_Analysis.Accept_Token (Token.Tok_Right_Parenthesis);
            I1 := I;




            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Cond,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Cond_Count => Condition_Count),
                            Position    => Pos_Sub);
            Pos := Pos_Sub;
            Condition_Count := Condition_Count + 1;

            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Not,
                                         Line => Lexical_Analysis.Get_Current_Token.Line),
                            Position    => Pos_Sub);
            Pos := Pos_Sub;

            Pos_Sub := Tree.First_Child (E2.Root);
            Tree.Splice_Subtree (Target   => N,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => E2,
                                 Position => Pos_Sub);

            Pos := Tree.Parent (Pos); -- get Cond from not

            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Break,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Loop_Count => Current_Loop_Nb));



            Pos := Tree.Parent (Pos); -- Get Loop from Cond
            Pos_Sub := Tree.First_Child (I1.Root);
            Tree.Splice_Subtree (Target   => N,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => I1,
                                 Position => Pos_Sub);


            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Label,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Loop_Count => Current_Loop_Nb),
                            Position    => Pos_Sub);
            Pos := Pos_Sub;

            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Drop,
                                         Line => Lexical_Analysis.Get_Current_Token.Line),
                            Position    => Pos_Sub);
            Pos := Pos_Sub;

            Pos_Sub := Tree.First_Child (E3.Root);
            Tree.Splice_Subtree (Target   => N,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => E3,
                                 Position => Pos_Sub);

            return N;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_Do) then
         declare
            N : Tree.Tree;
            I1 : Tree.Tree;
            E1 : Tree.Tree;
            Pos : Tree.Cursor;
            Pos_Sub : Tree.Cursor;
            Current_Loop_Nb : constant Positive := Loop_Count;
         begin
            N.Insert_Child (Parent   => N.Root,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Loop,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Loop_Count => Loop_Count),
                            Position    => Pos_Sub);
            Loop_Count := Loop_Count + 1;

            I1 := I;
            Lexical_Analysis.Accept_Token (Token.Tok_While);
            Lexical_Analysis.Accept_Token (Token.Tok_Left_Parenthesis);
            E1 := E;
            Lexical_Analysis.Accept_Token (Token.Tok_Right_Parenthesis);
            Lexical_Analysis.Accept_Token (Token.Tok_Semi_Colon);

            Pos := Pos_Sub;
            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Seq,
                                         Line => Lexical_Analysis.Get_Current_Token.Line),
                            Position    => Pos_Sub);
            Pos := Pos_Sub;
            Pos_Sub := Tree.First_Child (I1.Root);
            Tree.Splice_Subtree (Target   => N,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => I1,
                                 Position => Pos_Sub);

            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Label,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Loop_Count => Current_Loop_Nb),
                            Position    => Pos_Sub);

            Pos := Pos_Sub;
            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Cond,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Cond_Count => Condition_Count),
                            Position    => Pos_Sub);
            Condition_Count := Condition_Count + 1;

            Pos := Pos_Sub;
            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Not,
                                         Line => Lexical_Analysis.Get_Current_Token.Line),
                            Position    => Pos_Sub);

            Pos := Pos_Sub;
            Pos_Sub := Tree.First_Child (E1.Root);
            Tree.Splice_Subtree (Target   => N,
                                 Parent   => Pos,
                                 Before   => Tree.No_Element,
                                 Source   => E1,
                                 Position => Pos_Sub);

            Pos := Tree.Parent (Pos); -- Get Cond from Not
            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Break,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Loop_Count => Current_Loop_Nb),
                            Position    => Pos_Sub);
            return N;

         end;


      elsif Lexical_Analysis.Check_Token (Token.Tok_Continue) then
         declare
            N : Tree.Tree;
         begin
            N.Insert_Child (Parent   => N.Root,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Continue,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Loop_Count => Loop_Count - 1));
            Lexical_Analysis.Accept_Token (Token.Tok_Semi_Colon);
            return N;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_Break) then
         declare
            N : Tree.Tree;
         begin
            N.Insert_Child (Parent   => N.Root,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Break,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Loop_Count => Loop_Count - 1));
            Lexical_Analysis.Accept_Token (Token.Tok_Semi_Colon);
            return N;
         end;
      elsif Lexical_Analysis.Check_Token (Token.Tok_Return) then
         declare
            N : Tree.Tree;
            E1 : Tree.Tree := E;
            Pos : Tree.Cursor;
         begin
            Lexical_Analysis.Accept_Token (Token.Tok_Semi_Colon);
            N.Insert_Child (Parent   => N.Root,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Return,
                                         Line => Lexical_Analysis.Get_Current_Token.Line));
            Pos := Tree.First_Child (E1.Root);
            Tree.Splice_Subtree (Target   => N,
                                 Parent   => Tree.First_Child (N.Root),
                                 Before   => Tree.No_Element,
                                 Source   => E1,
                                 Position => Pos);
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

   function F return Tree.Tree is
   begin
      Lexical_Analysis.Accept_Token (Token.Tok_Int);
      Lexical_Analysis.Accept_Token (Token.Tok_Id);
      declare
         N : Tree.Tree;
         Pos : Tree.Cursor;
         Pos_Sub : Tree.Cursor;
         I1 : Tree.Tree;
         First_Loop : Boolean := True;
      begin
         N.Insert_Child (Parent   => N.Root,
                         Before   => Tree.No_Element,
                         New_Item => (Node_Type => Node_Body_Func,
                                      Line => Lexical_Analysis.Get_Current_Token.Line,
                                      Name_Key => Lexical_Analysis.Get_Current_Token.Value,
                                      Nb_Var => 0),
                         Position => Pos);

         N.Insert_Child (Parent   => Pos,
                         Before   => Tree.No_Element,
                         New_Item => (Node_Type => Node_Seq,
                                      Line => Lexical_Analysis.Get_Current_Token.Line),
                         Position    => Pos_Sub);
         Pos := Pos_Sub;
         Lexical_Analysis.Accept_Token (Token.Tok_Left_Parenthesis);
         while not Lexical_Analysis.Check_Token (Token.Tok_Right_Parenthesis) loop
            if First_Loop then
               First_Loop := False;
            else
               if (not Lexical_Analysis.Check_Token (Token.Tok_Comma)) then
                  Error_Log.Error (Msg  => "comma is missing",
                                   Line => Lexical_Analysis.Get_Current_Token.Line);
                  raise Error_Log.Compilation_Error;
               end if;
            end if;
            Lexical_Analysis.Accept_Token (Token.Tok_Int);
            Lexical_Analysis.Accept_Token (Token.Tok_Id);
            N.Insert_Child (Parent   => Pos,
                            Before   => Tree.No_Element,
                            New_Item => (Node_Type => Node_Var_Decl,
                                         Line => Lexical_Analysis.Get_Current_Token.Line,
                                         Var_Key => Lexical_Analysis.Get_Current_Token.Value));
         end loop;

         I1 := I;
         Pos := Tree.First_Child (N.Root);
         Pos_Sub := Tree.First_Child (I1.Root);
         Tree.Splice_Subtree (Target   => N,
                              Parent   => Pos,
                              Before   => Tree.No_Element,
                              Source   => I1,
                              Position => Pos_Sub);
         return N;
      end;
   end F;

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


begin
   -- Op table initialisation
   Op_Table (Token.Tok_Assignment) := (Left_Priority => 1,
                                 Right_Priority => 1,
                                 Node => Node_Op_Assignment);

   Op_Table (Token.Tok_Or_Boolean) := (Left_Priority => 2,
                                 Right_Priority => 3,
                                 Node => Node_Op_Or_Boolean);

   Op_Table (Token.Tok_And_Boolean) := (Left_Priority => 3,
                                  Right_Priority => 4,
                                  Node => Node_Op_And_Boolean);

   Op_Table (Token.Tok_Equal_Comparaison) := (Left_Priority => 4,
                                        Right_Priority => 5,
                                        Node => Node_Op_Equal_Comparaison);
   Op_Table (Token.Tok_Difference_Comparaison) := (Left_Priority => 4,
                                             Right_Priority => 5,
                                             Node => Node_Op_Difference_Comparaison);

   Op_Table (Token.Tok_Greater_than) := (Left_Priority => 5,
                                   Right_Priority => 6,
                                   Node => Node_Op_Greater_than);
   Op_Table (Token.Tok_Greater_Or_Equal_Than) := (Left_Priority => 5,
                                            Right_Priority => 6,
                                            Node => Node_Op_Greater_Or_Equal_Than);
   Op_Table (Token.Tok_Less_Than) := (Left_Priority => 5,
                                Right_Priority => 6,
                                Node => Node_Op_Less_Than);
   Op_Table (Token.Tok_Less_Or_Equal_Than) := (Left_Priority => 5,
                                         Right_Priority => 6,
                                         Node => Node_Op_Less_Or_Equal_Than);

   Op_Table (Token.Tok_Plus) := (Left_Priority => 6,
                           Right_Priority => 7,
                           Node => Node_Op_Add);
   Op_Table (Token.Tok_Minus) := (Left_Priority => 6,
                            Right_Priority => 7,
                            Node => Node_Op_Sub);

   Op_Table (Token.Tok_Asterisk) := (Left_Priority => 7,
                               Right_Priority => 8,
                               Node => Node_Op_Mult);
   Op_Table (Token.Tok_Slash) := (Left_Priority => 7,
                            Right_Priority => 8,
                            Node => Node_Op_Division);
   Op_Table (Token.Tok_Percent) := (Left_Priority => 7,
                              Right_Priority => 8,
                              Node => Node_Op_Modulo);

end Syntaxic_Analysis;

