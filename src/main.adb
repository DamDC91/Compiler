with Lexical_Analysis;
with Syntaxic_Analysis;
with Ada.Strings.Fixed;
with Asm_Generation;
with GNATCOLL.Opt_Parse;
with Ada.Strings.Unbounded;
with Ada.Directories;
with Semantic_Analysis;
with Error_Log;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Calendar;

procedure main is

   package Args is
      use GNATCOLL.Opt_Parse;
      Parser : Argument_Parser := Create_Argument_Parser (Help => "reduce C Compiler for msm");

      package Files is new Parse_Positional_Arg_List (Parser      => Parser,
                                                      Name        => "files",
                                                      Help        => "Files to compile",
                                                      Allow_Empty => True,
                                                      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String);

      package warning is new Parse_Flag (Parser  => Parser,
                                         Short   => "-w",
                                         Long    => "--warning",
                                         Help    => "Print warning");

      package debug is new Parse_Flag (Parser  => Parser,
                                       Short   => "-g",
                                       Long    => "--debug",
                                       Help    => "Print debug infos");

      package Output_Filename is new Parse_Option (Parser      => Parser,
                                                   Short       => "-o",
                                                   Long        => "--output",
                                                   Help        => "Output file name",
                                                   Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
                                                   Default_Val => Ada.Strings.Unbounded.To_Unbounded_String ("out.s"));

      package Output_Dirname is new Parse_Option (Parser      => Parser,
                                                   Short       => "-d",
                                                   Long        => "--output-dir",
                                                   Help        => "Output directorie name",
                                                   Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
                                                   Default_Val => Ada.Strings.Unbounded.To_Unbounded_String ("./"));

      package Compile_Runtime is new Parse_Flag (Parser  => Parser,
                                                 Short   => "-r",
                                                 Long    => "--compile_runtime",
                                                 Help    => "force runtime compilation");

   end Args;

begin

   if Args.Parser.Parse and then (Args.Files.Get'Length > 0 or Args.Compile_Runtime.Get) then

      declare
         Command : constant String := Ada.Directories.Full_Name (Ada.Command_Line.Command_Name);
         Output_Dir : constant String := Ada.Strings.Unbounded.To_String (Args.Output_Dirname.Get);
         Asm_Filename : constant String := Output_Dir & "/" & Ada.Strings.Unbounded.To_String (Args.Output_Filename.Get);
         Id : constant Natural := Ada.Strings.Fixed.Index (Source  => Command,
                                                           Pattern => "/",
                                                           From    => Command'Last,
                                                           Going   => Ada.Strings.Backward);

         Runtime_Dir : constant String := (if Id /= 0 then Command(Command'First .. Id) & "../runtime/" else "../runtime/");
         Runtime_Source_File : constant String := Runtime_Dir & "runtime.c";
         Runtime_Asm_File : constant String := Runtime_Dir & "runtime.s";
         Start_File : constant string := Runtime_Dir & "start.s";
         use type Ada.Calendar.Time;
         use type Ada.Directories.File_Kind;
      begin

         if not Ada.Directories.Exists (Runtime_Dir) then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Runtime directory is not found");
            raise Error_Log.Internal_Error;

         elsif not Ada.Directories.Exists (Runtime_Source_File) then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Runtime source file is not found");
            raise Error_Log.Internal_Error;

         elsif not Ada.Directories.Exists (Start_File) then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "start.s is not found");
            raise Error_Log.Internal_Error;

         elsif not Ada.Directories.Exists (Output_Dir) or else Ada.Directories.Kind (Output_Dir) /= Ada.Directories.Directory then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Output directory doesn't exist");
            raise Error_Log.Internal_Error;

         elsif Ada.Strings.Unbounded.To_String (Args.Output_Filename.Get) /= Ada.Directories.Simple_Name (Ada.Strings.Unbounded.To_String (Args.Output_Filename.Get)) then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "invalid output file name");
            raise Error_Log.Internal_Error;
         end if;

         Error_Log.Set_Output_Dir (Output_Dir);
         -- Compiling runtime only if the code as been touch since the last compilation
         Error_Log.Set_Debug_On (Args.Compile_Runtime.Get and Args.debug.Get);
          -- removing /bin/../runtime if runtime.s already exist
         Error_Log.Set_Filename ((if Ada.Directories.Exists (Runtime_Asm_File) then Ada.Directories.Full_Name (Runtime_Source_File) else Runtime_Source_File));
         Lexical_Analysis.Load (Runtime_Source_File);

         declare
            T : Syntaxic_Analysis.Tree.Tree := Syntaxic_Analysis.G;
         begin
            -- only run syntaxic and semantic analysis for setting loop and condition counter and the symbol table
            Semantic_Analysis.AST_Analyse (T);

            if Args.Compile_Runtime.Get then
               Asm_Generation.Create_File(Filename => Runtime_Asm_File);
               Asm_Generation.Generate_Asm (Syntaxic_Analysis.Tree.First_Child (T.Root));
               Asm_Generation.Close_File;
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output, "Runtime compilation succeed");

            elsif Ada.Directories.Exists (Runtime_Asm_File) and then Ada.Directories.Modification_Time (Runtime_Asm_File) < Ada.Directories.Modification_Time (Runtime_Source_File) then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output, "Runtime has been modified since last compilation, should be compile");

            elsif not Ada.Directories.Exists (Runtime_Asm_File) then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Runtime need to be compile first, please use the -r option");
               raise Error_Log.Internal_Error;
            end if;
         end;

         if (Args.Files.Get'Length > 0) then
            Asm_Generation.Create_File (Asm_Filename);

            declare
               Files_Array   : constant Args.Files.Result_Array := Args.Files.Get;
            begin
               Error_Log.Set_Warning_On (Args.warning.Get);
               for i in Files_Array'First ..Files_Array'Last loop

                  declare
                     FileName : constant String := Ada.Strings.Unbounded.To_String (Files_Array (i));
                  begin

                     if not Ada.Directories.Exists (FileName) then
                        Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "error : '" & FileName & "': not such file");
                        raise Error_Log.Internal_Error;
                     end if;
                     Error_Log.Set_Filename  (FileName);
                     Error_Log.Set_Debug_On (Args.debug.Get);
                     Lexical_Analysis.Load(FileName);
                     declare
                        T : Syntaxic_Analysis.Tree.Tree := Syntaxic_Analysis.G;
                     begin
                        Semantic_Analysis.AST_Analyse (T);
                        Asm_Generation.Generate_Asm (Syntaxic_Analysis.Tree.First_Child (T.Root));
                     end;
                  end;
               end loop;
            end;

            Asm_Generation.Add_Runtime (Runtime  => Runtime_Asm_File);
            Asm_Generation.Add_Start (Start_Filename => Start_File);

            Asm_Generation.Close_File;
         end if;
      end;
      if Args.Files.Get'Length > 0 then
         Ada.Text_IO.Put_Line ("Compilation succeed");
      end if;
   else
      Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Output, Args.Parser.Help);
   end if;

exception
   when Error_Log.Input_Error =>
      Asm_Generation.Delete_File;
      Error_Log.Close_Token_File;
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Compilation failed");

   when Error_Log.Internal_Error =>
      Asm_Generation.Delete_File;
      Error_Log.Close_Token_File;
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Compitlation failled");

   when others =>
      Asm_Generation.Delete_File;
      Error_Log.Close_Token_File;
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Bug report : please send a email with a reproducer at damien.de-campos@u-psud.fr");
      raise;
end main;
