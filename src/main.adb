with Ada.Text_IO;
use Ada.Text_IO;
with Token;
with Text_Utils;
with Lexical_Analysis;



procedure main is

begin
   Put_Line (Text_Utils.Get_File_Content("test.c"));
   Lexical_Analysis.Load("test.c");
   while not Lexical_Analysis.EOF loop
      Lexical_Analysis.Advance_Token;
   end loop;
end main;
