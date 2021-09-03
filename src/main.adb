with Ada.Text_IO;
use Ada.Text_IO;
with Token;
with Text_Utils;
with Lexical_Analysis;
with Syntaxic_Analysis;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
with Ada.Exceptions;
with Ada.Command_Line;

procedure main is

   procedure process (C : Syntaxic_Analysis.Tree.Cursor) is
      N : constant Syntaxic_Analysis.Node := Syntaxic_Analysis.Tree.Element (C);
      P : constant Natural := Natural (Syntaxic_Analysis.Tree.Depth (C))-2;
      str : constant string := P * "--";
   begin
      Ada.Text_IO.Put(str);
      Syntaxic_Analysis.Debug_Print (N);
   end process;

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line (Standard_Error, "Invalid file");
      return;
   end if;

   for i in 1..Ada.Command_Line.Argument_Count loop
      declare
         FileName : constant String := Ada.Command_Line.Argument(i);
      begin

         Lexical_Analysis.Load(FileName);

         declare
            T : Syntaxic_Analysis.Tree.Tree := Syntaxic_Analysis.G;
         begin
            Syntaxic_Analysis.Tree.Iterate(T,process'Access);
         end;
      end;
   end loop;
end main;
