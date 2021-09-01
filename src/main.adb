with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
with tocken;
procedure main is

   function Get_File_Content(FileName : String) return String is
      File : Ada.Text_IO.File_Type;
      str : ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   begin
      Ada.Text_IO.Open(File => File,
                       Mode => Ada.Text_IO.In_File,
                       Name => FileName);
      while not End_Of_File(File) loop
         Ada.Strings.Unbounded.Append(Str, Get_Line(File));
      end loop;
      return Ada.Strings.Unbounded.To_String(Str);
   end Get_File_Content;

begin
   null;

end main;
