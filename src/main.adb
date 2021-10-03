with Lexical_Analysis;
with Syntaxic_Analysis;
with Ada.Strings.Fixed;
with Asm_Generation;
with GNATCOLL.Opt_Parse;
with Ada.Strings.Unbounded;
with Ada.Directories;
with Semantic_Analysis;
with Error_Log;

procedure main is

   package Args is
      use GNATCOLL.Opt_Parse;
      Parser : Argument_Parser := Create_Argument_Parser (Help => "reduce C Compiler for msm");

      package Files is new Parse_Positional_Arg_List (Parser      => Parser,
                                                      Name        => "files",
                                                      Help        => "Files to compile",
                                                      Allow_Empty => False,
                                                      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String);

      package debug is new Parse_Flag (Parser  => Parser,
                                       Short   => "-d",
                                       Long    => "--debug",
                                       Help    => "Print debug infos");

   end Args;

begin

   if Args.Parser.Parse then
      declare
         Is_Debug_Mode : constant Boolean := Args.debug.Get;
         Files_Array   : constant Args.Files.Result_Array := Args.Files.Get;

      begin
         Syntaxic_Analysis.Init;

         for i in Files_Array'First ..Files_Array'Last loop

            declare
               FileName : constant String := Ada.Strings.Unbounded.To_String (Files_Array (i));
               Simple_FileName : constant String := Ada.Directories.Simple_Name (FileName);
               Index : constant Integer := Ada.Strings.Fixed.Index (Source  => Simple_FileName,
                                                                    Pattern => ".c") - 1;
               Asm_FileName : constant String := Simple_FileName (Simple_FileName'First..Index) & ".asm";

            begin
               Error_Log.Set_Filename  (FileName);
               Lexical_Analysis.Load(FileName, Is_Debug_Mode);


               declare
                  T : Syntaxic_Analysis.Tree.Tree := Syntaxic_Analysis.G;
               begin
                  Semantic_Analysis.AST_Analyse (T);

                  Asm_Generation.Create_File(Asm_FileName); -- need to be created here because it uses Nb_Var from Semantic_Analyse

                  Asm_Generation.Generate_Asm (Syntaxic_Analysis.Tree.First_Child (T.Root));

                  if Is_Debug_Mode then
                     Syntaxic_Analysis.Debug_Print_Tree (T);
                     Syntaxic_Analysis.Debug_Print_Tree_Graphviz (T);
                  end if;


               end;

            end;
            Lexical_Analysis.Close_Debug;
            Asm_Generation.Close_File;
         end loop;


      end;
   end if;

end main;
