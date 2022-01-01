with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Error_Log;
with Ada.Exceptions;
package body Text_Utils is

   function Get_File_Content(FileName : String) return String is
      File : Ada.Text_IO.File_Type;
      str : ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      use Ada.Text_IO;
   begin
      begin
         Ada.Text_IO.Open(File => File,
                          Mode => Ada.Text_IO.In_File,
                          Name => FileName);
      exception
         when e : others =>
            Put_Line (Standard_Error, "error : '" & FileName & "' cannot be open");
            Put_Line (Standard_Error, Ada.Exceptions.Exception_Message (e));
            raise Error_Log.Internal_Error;
      end;

      while not End_Of_File(File) loop
         Ada.Strings.Unbounded.Append(Str, Get_Line(File) & Ada.Characters.Latin_1.LF);
      end loop;
      Ada.Text_IO.Close (File);
      return Ada.Strings.Unbounded.To_String(Str) & Ada.Characters.Latin_1.EOT;
   end Get_File_Content;

end Text_Utils;
