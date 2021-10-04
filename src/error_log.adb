with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
package body Error_Log is

   Current_FileName : Unbounded_String;

   procedure Set_Filename(str : string) is
   begin
      Current_FileName := To_Unbounded_String (Str);
   end Set_Filename;

   procedure Error (Msg : String; Line : Natural) is
      FileName : constant string := To_String (Current_FileName);
      File : File_Type;
   begin
      Put_Line (Standard_Error, FileName & ":" & Line'Image & ": error " & Msg);

      Open(File, In_File, FileName);
      if Line > 1 then
         Skip_Line (File => File,
                    Spacing => Ada.Text_IO.Count (Line-1));
      end if;
      Put_Line(Standard_Error, "   " & Line'Image & " | " & Get_Line(File));

      Close(File);
   end Error;


   procedure Warning (Msg : String; Line : Natural) is
      FileName : constant string := To_String (Current_FileName);
      File : File_Type;
   begin
      Put_Line (Standard_Error, FileName & ":" & Line'Image & ": Warning " & Msg);

      Open(File, In_File, FileName);
      if Line > 1 then
         Skip_Line (File => File,
                    Spacing => Ada.Text_IO.Count (Line-1));
      end if;
      Put_Line(Standard_Error, "   " & Line'Image & " | " & Get_Line(File));

      Close(File);
   end Warning;

end Error_Log;
