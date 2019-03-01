with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
use Ada;
with Unchecked_Deallocation;

with GNAT.Command_Line;
--  We don't use most of the facilities of GNAT.Command_Line.
--  We use it mainly for the wildcard-expansion facility.

with Utils.Tool_Names;
with Utils.Strings; use Utils.Strings;

package body Utils.Command_Lines is
   use Text_IO;

   procedure Free is new Unchecked_Deallocation (String, String_Ref);

   procedure Raise_Cmd_Error (Message : String) is
   begin
      raise Command_Line_Error with Message;
   end Raise_Cmd_Error;

   procedure Cmd_Error (Message : String) is
   begin
      Put (Standard_Error, Utils.Tool_Names.Tool_Name & ": ");
      Put_Line (Standard_Error, Message);
      raise Command_Line_Error with Message;
   end Cmd_Error;

   procedure Cmd_Error_No_Help (Message : String) is
   begin
      Put (Standard_Error, Utils.Tool_Names.Tool_Name & ": ");
      Put_Line (Standard_Error, Message);
      raise Command_Line_Error_No_Help with Message;
   end Cmd_Error_No_Help;

   procedure Cmd_Error_No_Tool_Name (Message : String) is
   begin
      Put_Line (Standard_Error, Message);
      raise Command_Line_Error_No_Tool_Name with Message;
   end Cmd_Error_No_Tool_Name;

   function Text_Args_From_Command_Line return Argument_List_Access is
      use Ada.Command_Line;
      Result : String_Access_Vector;

      Cur : Positive := 1;
   begin
      while Cur <= Argument_Count loop
         --  We skip "-x ada", which is passed in by gprbuild. It is necessary
         --  to do that here, rather than during the command-line parsing,
         --  because gnatmetric uses the -x switch for something else.

         if Argument (Cur) = "-x" and then Cur < Argument_Count
           and then Argument (Cur + 1) = "ada"
         then
            Cur := Cur + 2;

            --  We also skip -cargs sections. For gnatcheck, we need to deal
            --  with -rules sections. ????Perhaps this should be parameterized
            --  by sections to skip and section to pay attention to.

         elsif Argument (Cur) = "-cargs"
           or else Argument (Cur) = "-inner-cargs"
         then
            --  Skip until we get to the end or see "-asis-tool-args"
            --  Shouldn't we stop at "-rules" as well????

            loop
               Cur := Cur + 1;
               if Cur > Argument_Count then
                  exit;
               end if;
               if Argument (Cur) = "-asis-tool-args" then
                  Cur := Cur + 1;
                  exit;
               end if;
            end loop;

         elsif Argument (Cur) = "-asis-tool-args" then
            Cur := Cur + 1;

         else
            Append (Result, new String'(Argument (Cur)));
            Cur := Cur + 1;
         end if;
      end loop;

      return new Argument_List'(To_Array (Result));
   end Text_Args_From_Command_Line;

   function Text_Cargs_From_Command_Line return Argument_List_Access is
      use Ada.Command_Line;
      Result : String_Access_Vector;

      Cur : Positive := 1;
   begin
      while Cur <= Argument_Count loop
         if Argument (Cur) = "-cargs" or else Argument (Cur) = "-inner-cargs"
         then
            --  Append until we get to the end or see "-asis-tool-args"
            --  Shouldn't we stop at "-rules" as well????

            loop
               Append
                 (Result,
                  new String'
                    (if Argument (Cur) = "-inner-cargs" then "-cargs"
                     else Argument (Cur)));
               Cur := Cur + 1;
               if Cur > Argument_Count then
                  exit;
               end if;
               if Argument (Cur) = "-asis-tool-args" then
                  Cur := Cur + 1;
                  exit;
               end if;
            end loop;
         else -- skip
            Cur := Cur + 1;
         end if;
      end loop;

      return new Argument_List'(To_Array (Result));
   end Text_Cargs_From_Command_Line;

   generic
      type Switches is (<>);
   function Generic_Switch_Text (Switch : Switches) return String_Ref;

   generic
      type Switches is (<>);
   function Generic_Negated_Switch_Text (Switch : Switches) return String_Ref;

   function Generic_Switch_Text (Switch : Switches) return String_Ref is
   begin
      return +("--" & Dashes (To_Lower (Switches'Image (Switch))));
   end Generic_Switch_Text;

   function Generic_Negated_Switch_Text (Switch : Switches) return String_Ref
   is
   begin
      return +("--no-" & Dashes (To_Lower (Switches'Image (Switch))));
   end Generic_Negated_Switch_Text;

   function Error_Detected (Cmd : Command_Line) return Boolean is
     (Cmd.Error_Detected);

   --  Efficiency: Command_Line_Rec contains entries for all the short-hand
   --  switches, but those are never used.

   package body Flag_Switches is
      pragma Assert (Descriptor.Allowed_Switches = null);
      --  Assert that we're not adding switches after Freeze_Descriptor

      function Arg (Cmd : Command_Line; Switch : Switches) return Boolean is
      begin
         return Cmd.Sw (To_All (Switch)).Boolean_Val;
      end Arg;

      procedure Set_Arg
        (Cmd : in out Command_Line; Switch : Switches; Val : Boolean := True)
      is
      begin
         Cmd.Sw (To_All (Switch)).Boolean_Val := Val;
      end Set_Arg;

      package body Set_Shorthands is
         pragma Assert (Descriptor.Allowed_Switches = null);
      begin
         for J in Shorthands'Range loop
            if Shorthands (J) /= null then
               pragma Assert (Shorthands (J) (1) = '-');
               Append
                 (Descriptor.Allowed_Switches_Vector,
                  Switch_Descriptor'
                    (Kind  => True_Switch, Text => Shorthands (J),
                     Alias => To_All (J), others => <>));
            end if;
         end loop;
      end Set_Shorthands;

      Base_Switch : constant All_Switches :=
        Last_Index (Descriptor.Allowed_Switches_Vector) + 1;

      function To_All (Switch : Switches) return All_Switches is
        (Base_Switch + Switches'Pos (Switch));

      function From_All (Switch : All_Switches) return Switches is
        (Switches'Val (Switch - Base_Switch));

      function Valid (Switch : All_Switches) return Boolean is
        (Switch in To_All (Switches'First) .. To_All (Switches'Last));

      function Switch_Text is new Generic_Switch_Text (Switches);

   begin
      for Switch in Switches loop
         Append
           (Descriptor.Allowed_Switches_Vector,
            Switch_Descriptor'
              (Kind  => True_Switch, Text => Switch_Text (Switch),
               Alias => To_All (Switch), others => <>));
      end loop;
   end Flag_Switches;

   package body Boolean_Switches is
      pragma Assert (Descriptor.Allowed_Switches = null);

      function Arg (Cmd : Command_Line; Switch : Switches) return Boolean is
      begin
         return Cmd.Sw (To_All (Switch)).Boolean_Val;
      end Arg;

      procedure Set_Arg
        (Cmd : in out Command_Line; Switch : Switches; Val : Boolean := True)
      is
      begin
         Cmd.Sw (To_All (Switch)).Boolean_Val := Val;
      end Set_Arg;

      function Explicit (Cmd : Command_Line; Switch : Switches) return Boolean
      is
      begin
         return Cmd.Sw (To_All (Switch)).Explicit;
      end Explicit;

      package body Set_Defaults is
         pragma Assert (Descriptor.Allowed_Switches = null);
      begin
         for J in Defaults'Range loop
            Descriptor.Allowed_Switches_Vector (To_All (J)).Default_Bool :=
              Defaults (J);
         end loop;
      end Set_Defaults;

      package body Set_Shorthands is
         pragma Assert (Descriptor.Allowed_Switches = null);
         After_Dashes : Natural := 0;
      begin
         for J in Shorthands'Range loop
            if Shorthands (J) /= null then
               if Shorthands (J) (1 .. 2) = "--" then
                  After_Dashes := 3;
               elsif Shorthands (J) (1 .. 1) = "-" then
                  After_Dashes := 2;
               else
                  pragma Assert (False);
               end if;

               Append
                 (Descriptor.Allowed_Switches_Vector,
                  Switch_Descriptor'
                    (Kind  => True_Switch, Text => Shorthands (J),
                     Alias => To_All (J), others => <>));

               Append
                 (Descriptor.Allowed_Switches_Vector,
                  Switch_Descriptor'
                    (Kind => False_Switch,
                     Text =>
                       new String'
                         ("--no-" &
                          Shorthands (J)
                            (After_Dashes .. Shorthands (J)'Last)),
                     --  So if the shorthand is "--foo-bar" or "-foo-bar",
                     --  this is "--no-foo-bar".

                     Alias => To_All (J), others => <>));
            end if;
         end loop;
      end Set_Shorthands;

      Base_Switch : constant All_Switches :=
        Last_Index (Descriptor.Allowed_Switches_Vector) + 1;

      function To_All (Switch : Switches) return All_Switches is
        (Base_Switch + Switches'Pos (Switch));

      function From_All (Switch : All_Switches) return Switches is
        (Switches'Val (Switch - Base_Switch));

      function Valid (Switch : All_Switches) return Boolean is
        (Switch in To_All (Switches'First) .. To_All (Switches'Last));

      function Switch_Text is new Generic_Switch_Text (Switches);
      function Negated_Switch_Text is new Generic_Negated_Switch_Text
        (Switches);

   begin
      for Switch in Switches loop
         Append
           (Descriptor.Allowed_Switches_Vector,
            Switch_Descriptor'
              (Kind  => True_Switch, Text => Switch_Text (Switch),
               Alias => To_All (Switch), others => <>));
      end loop;

      for Switch in Switches loop
         Append
           (Descriptor.Allowed_Switches_Vector,
            Switch_Descriptor'
              (Kind  => False_Switch, Text => Negated_Switch_Text (Switch),
               Alias => To_All (Switch), others => <>));
      end loop;
   end Boolean_Switches;

   package body Enum_Switches is
      pragma Assert (Descriptor.Allowed_Switches = null);

      function Arg (Cmd : Command_Line) return Switches is
      begin
         return Result : Switches := Default do
            --  Default is Switches'First, which will be overwritten below if
            --  any of the switches were explicitly specified.

            for Switch in Switches loop
               if Cmd.Sw (To_All (Switch)).Position >
                 Cmd.Sw (To_All (Result)).Position
               then
                  Result := Switch;
               end if;
            end loop;
         end return;
      end Arg;

      procedure Set_Arg (Cmd : in out Command_Line; Switch : Switches) is
         Current_Setting : constant Switches := Arg (Cmd);
      begin
         --  Set the Position to one more than the position of the current
         --  switch setting. This will cause Arg (Cmd) to return Switch.

         Cmd.Sw (To_All (Switch)).Position :=
           Cmd.Sw (To_All (Current_Setting)).Position + 1;
      end Set_Arg;

      package body Set_Shorthands is
         pragma Assert (Descriptor.Allowed_Switches = null);
      begin
         for J in Shorthands'Range loop
            if Shorthands (J) /= null then
               pragma Assert (Shorthands (J) (1) = '-');
               Append
                 (Descriptor.Allowed_Switches_Vector,
                  Switch_Descriptor'
                    (Kind  => Enum_Switch, Text => Shorthands (J),
                     Alias => To_All (J), others => <>));
            end if;
         end loop;
      end Set_Shorthands;

      Base_Switch : constant All_Switches :=
        Last_Index (Descriptor.Allowed_Switches_Vector) + 1;

      function To_All (Switch : Switches) return All_Switches is
        (Base_Switch + Switches'Pos (Switch));

      function From_All (Switch : All_Switches) return Switches is
        (Switches'Val (Switch - Base_Switch));

      function Valid (Switch : All_Switches) return Boolean is
        (Switch in To_All (Switches'First) .. To_All (Switches'Last));

      function Switch_Text is new Generic_Switch_Text (Switches);

   begin
      for Switch in Switches loop
         Append
           (Descriptor.Allowed_Switches_Vector,
            Switch_Descriptor'
              (Kind  => Enum_Switch, Text => Switch_Text (Switch),
               Alias => To_All (Switch), others => <>));
      end loop;
   end Enum_Switches;

   package body String_Switches is
      pragma Assert (Descriptor.Allowed_Switches = null);

      function Arg (Cmd : Command_Line; Switch : Switches) return String_Ref is
      begin
         return Cmd.Sw (To_All (Switch)).String_Val;
      end Arg;

      procedure Set_Arg
        (Cmd : in out Command_Line; Switch : Switches; Val : String)
      is
      begin
         Cmd.Sw (To_All (Switch)).String_Val := new String'(Val);
      end Set_Arg;

      Set_Shorthands_Instantiated : Boolean := False;

      package body Set_Syntax is
         pragma Assert (Descriptor.Allowed_Switches = null);
         pragma Assert (not Set_Shorthands_Instantiated);
      begin
         for J in Syntax'Range loop
            Descriptor.Allowed_Switches_Vector (To_All (J)).Syntax :=
              Syntax (J);
         end loop;
      end Set_Syntax;

      package body Set_Defaults is
         pragma Assert (Descriptor.Allowed_Switches = null);
      begin
         for J in Defaults'Range loop
            Descriptor.Allowed_Switches_Vector (To_All (J)).Default :=
              Defaults (J);
         end loop;
      end Set_Defaults;

      package body Set_Shorthands is
         pragma Assert (Descriptor.Allowed_Switches = null);
      begin
         Set_Shorthands_Instantiated := True;

         for J in Shorthands'Range loop
            if Shorthands (J) /= null then
               pragma Assert (Shorthands (J) (1) = '-');
               Append
                 (Descriptor.Allowed_Switches_Vector,
                  Switch_Descriptor'
                    (Kind  => String_Switch, Text => Shorthands (J),
                     Alias => To_All (J), others => <>));
            end if;
         end loop;
      end Set_Shorthands;

      Base_Switch : constant All_Switches :=
        Last_Index (Descriptor.Allowed_Switches_Vector) + 1;

      function To_All (Switch : Switches) return All_Switches is
        (Base_Switch + Switches'Pos (Switch));

      function From_All (Switch : All_Switches) return Switches is
        (Switches'Val (Switch - Base_Switch));

      function Valid (Switch : All_Switches) return Boolean is
        (Switch in To_All (Switches'First) .. To_All (Switches'Last));

      function Switch_Text is new Generic_Switch_Text (Switches);

   begin
      for Switch in Switches loop
         Append
           (Descriptor.Allowed_Switches_Vector,
            Switch_Descriptor'
              (Kind  => String_Switch, Text => Switch_Text (Switch),
               Alias => To_All (Switch), others => <>));
      end loop;
   end String_Switches;

   package body String_Seq_Switches is
      pragma Assert (Descriptor.Allowed_Switches = null);

      function Arg
        (Cmd : Command_Line; Switch : Switches) return String_Ref_Array
      is
      begin
         return To_Array (Cmd.Sw (To_All (Switch)).Seq_Val);
      end Arg;

      procedure Set_Arg
        (Cmd : in out Command_Line; Switch : Switches; Val : String_Ref_Array)
      is
      begin
         Clear (Cmd.Sw (To_All (Switch)).Seq_Val);
         Append (Cmd.Sw (To_All (Switch)).Seq_Val, Val);
      end Set_Arg;

      function Arg_Length
        (Cmd : Command_Line; Switch : Switches) return Natural
      is
      begin
         return Last_Index (Cmd.Sw (To_All (Switch)).Seq_Val);
      end Arg_Length;

      Set_Shorthands_Instantiated : Boolean := False;

      package body Set_Syntax is
         pragma Assert (Descriptor.Allowed_Switches = null);
         pragma Assert (not Set_Shorthands_Instantiated);
      begin
         for J in Syntax'Range loop
            Descriptor.Allowed_Switches_Vector (To_All (J)).Syntax :=
              Syntax (J);
         end loop;
      end Set_Syntax;

      package body Set_Shorthands is
         pragma Assert (Descriptor.Allowed_Switches = null);
      begin
         Set_Shorthands_Instantiated := True;

         for J in Shorthands'Range loop
            if Shorthands (J) /= null then
               pragma Assert (Shorthands (J) (1) = '-');
               Append
                 (Descriptor.Allowed_Switches_Vector,
                  Switch_Descriptor'
                    (Kind  => String_Seq_Switch, Text => Shorthands (J),
                     Alias => To_All (J), others => <>));
            end if;
         end loop;
      end Set_Shorthands;

      Base_Switch : constant All_Switches :=
        Last_Index (Descriptor.Allowed_Switches_Vector) + 1;

      function To_All (Switch : Switches) return All_Switches is
        (Base_Switch + Switches'Pos (Switch));

      function From_All (Switch : All_Switches) return Switches is
        (Switches'Val (Switch - Base_Switch));

      function Valid (Switch : All_Switches) return Boolean is
        (Switch in To_All (Switches'First) .. To_All (Switches'Last));

      function Switch_Text is new Generic_Switch_Text (Switches);

   begin
      for Switch in Switches loop
         Append
           (Descriptor.Allowed_Switches_Vector,
            Switch_Descriptor'
              (Kind  => String_Seq_Switch, Text => Switch_Text (Switch),
               Alias => To_All (Switch), others => <>));
      end loop;
   end String_Seq_Switches;

   package body Other_Switches is
      pragma Assert (Descriptor.Allowed_Switches = null);

      function Arg (Cmd : Command_Line; Switch : Switches) return Arg_Type is
      begin
         return Value (Cmd.Sw (To_All (Switch)).String_Val.all);
      --  If the ".all" blows up because of a null pointer, that's because
      --  the client forgot to instantiate Set_Defaults to set defaults.
      end Arg;

      procedure Set_Arg
        (Cmd : in out Command_Line; Switch : Switches; Val : Arg_Type)
      is
         S : constant String := Image (Val);
      begin
         if Cmd.Sw (To_All (Switch)).String_Val = null
           or else Cmd.Sw (To_All (Switch)).String_Val.all /= S
         then
            --  We don't Free the old value, here, because that
            --  would require Copy_Command_Line to do a deep copy,
            --  which would probably waste more memory than we're
            --  leaking here; this procedure is called very rarely.

            Cmd.Sw (To_All (Switch)).String_Val := new String'(S);
         end if;
      end Set_Arg;

      Set_Shorthands_Instantiated : Boolean := False;

      package body Set_Syntax is
         pragma Assert (Descriptor.Allowed_Switches = null);
         pragma Assert (not Set_Shorthands_Instantiated);
      begin
         for J in Syntax'Range loop
            Descriptor.Allowed_Switches_Vector (To_All (J)).Syntax :=
              Syntax (J);
         end loop;
      end Set_Syntax;

      package body Set_Defaults is
         pragma Assert (Descriptor.Allowed_Switches = null);
      begin
         for J in Defaults'Range loop
            Descriptor.Allowed_Switches_Vector (To_All (J)).Default :=
              +Image (Defaults (J));
         end loop;
      end Set_Defaults;

      package body Set_Shorthands is
         pragma Assert (Descriptor.Allowed_Switches = null);
      begin
         Set_Shorthands_Instantiated := True;

         for J in Shorthands'Range loop
            if Shorthands (J) /= null then
               pragma Assert (Shorthands (J) (1) = '-');
               Append
                 (Descriptor.Allowed_Switches_Vector,
                  Switch_Descriptor'
                    (Kind  => String_Switch, Text => Shorthands (J),
                     Alias => To_All (J), others => <>));
            end if;
         end loop;
      end Set_Shorthands;

      Base_Switch : constant All_Switches :=
        Last_Index (Descriptor.Allowed_Switches_Vector) + 1;

      function To_All (Switch : Switches) return All_Switches is
        (Base_Switch + Switches'Pos (Switch));

      function From_All (Switch : All_Switches) return Switches is
        (Switches'Val (Switch - Base_Switch));

      function Valid (Switch : All_Switches) return Boolean is
        (Switch in To_All (Switches'First) .. To_All (Switches'Last));

      function Switch_Text is new Generic_Switch_Text (Switches);

      procedure Validate (Text : String) is
      begin
         --  Value will raise Constraint_Error if Text is malformed. We don't
         --  want to just call it and throw away the result, because that might
         --  get optimized away. We also need to check that Value returns a
         --  value in Arg_Type; for example:
            --     Natural'Value
            --  can return a value outside Natural.

         if Value (Text) not in Arg_Type then
            raise Constraint_Error;
         end if;

      exception
         when Constraint_Error =>
            Raise_Cmd_Error ("Malformed argument: " & Text);
      end Validate;

   begin
      for Switch in Switches loop
         Append
           (Descriptor.Allowed_Switches_Vector,
            Switch_Descriptor'
              (Kind   => String_Switch, Text => Switch_Text (Switch),
               Alias  => To_All (Switch), Validator => Validate_Access,
               others => <>));
      end loop;
   end Other_Switches;

   package body Disable_Switches is
   begin
      pragma Assert (Descriptor.Allowed_Switches = null);
      --  Do this before Freeze_Descriptor

      for Switch of Disable loop
         Descriptor.Allowed_Switches_Vector (Switch).Enabled := False;
      end loop;
   end Disable_Switches;

   package body Freeze_Descriptor is
      --      pragma Assert (Descriptor.Allowed_Switches = null);
      --  Don't call this twice. Commented out because of kludgery
      --  in Metrics.Command_Lines. We should move the freezing
      --  of common switches into Common. ????

      procedure Do_It (Descriptor : in out Command_Line_Descriptor);
      --  Do most of the work in a procedure so the debugger works.

      procedure Do_It (Descriptor : in out Command_Line_Descriptor) is
         OK : Boolean := True;
      begin
         --  Validate that the allowed switches make sense. We can't have two
         --  switches with the same name. We don't want to have one switch a
         --  prefix of another if one of them is a String_Switch, and the
         --  syntax is not '='. For example, if we allowed "--outputparm" and
         --  "--output-dirparm", then we can't tell whether the latter is
         --  "--output-dir" with parameter "parm", or "--output" with parameter
         --  "-dirparm".
            --
         --  However, there is are cases where we have to allow this sort of
         --  thing for compatibility reasons: gnatstub has a string switch
         --  -gnaty, which is a prefix of -gnatyo and -gnatyM. Hence the
         --  special case for -gnaty below. In this case, Text_To_Switch will
         --  return the longest one (e.g. -gnatyM123 is -gnatyM with an
         --  argument of 123).

         for Switch1 in Descriptor.Allowed_Switches'Range loop
            pragma Assert
              (Descriptor.Allowed_Switches (Switch1).Kind /= No_Such);

            for Switch2 in Descriptor.Allowed_Switches'Range loop
               if Switch1 /= Switch2 and then Enabled (Descriptor, Switch1)
                 and then Enabled (Descriptor, Switch2)
               then
                  declare
                     Desc1 : Switch_Descriptor renames
                       Descriptor.Allowed_Switches (Switch1);
                     Desc2 : Switch_Descriptor renames
                       Descriptor.Allowed_Switches (Switch2);
                  begin
                     if Desc1.Text.all = Desc2.Text.all then
                        OK := False;
                        Put_Line
                          (Standard_Error, "duplicate " & Desc1.Text.all);
                     end if;

                     if Desc1.Kind in String_Switch | String_Seq_Switch
                       and then Syntax (Descriptor, Switch1) /= '='
                       and then Has_Prefix (Desc2.Text.all, Desc1.Text.all)
                       and then Desc1.Text.all /= "-gnaty"
                     then
                        OK := False;
                        Put_Line
                          (Standard_Error,
                           Desc1.Text.all & " is prefix of " & Desc2.Text.all);
                     end if;
                  end;
               end if;
            end loop;
         end loop;

         pragma Assert (OK);
      end Do_It;

   begin
      if Descriptor.Allowed_Switches = null then -- ???Replaces above assertion
         --  Switch over to using Allowed_Switches from Allowed_Switches_Vector

         Descriptor.Allowed_Switches :=
           new Switch_Descriptor_Array'
             (To_Array (Descriptor.Allowed_Switches_Vector));
         Free (Descriptor.Allowed_Switches_Vector);
         Do_It (Descriptor);
      end if;
   end Freeze_Descriptor;

   function Copy_Descriptor
     (Descriptor : Command_Line_Descriptor) return Command_Line_Descriptor
   is
   begin
      pragma Assert (Descriptor.Allowed_Switches /= null);
      pragma Assert (Is_Empty (Descriptor.Allowed_Switches_Vector));
      --  Assert that Freeze_Descriptor has been called

      return Result : Command_Line_Descriptor do
         Append
           (Result.Allowed_Switches_Vector, Descriptor.Allowed_Switches.all);
      end return;
   end Copy_Descriptor;

   function Copy_Command_Line (Cmd : Command_Line) return Command_Line is
   begin
      return Result : Command_Line (Cmd.Descriptor) do
         Result.File_Names       := Cmd.File_Names;
         Result.Current_Position := Cmd.Current_Position;
         Result.Sw := new Dynamically_Typed_Switches'(Cmd.Sw.all);
         Result.Error_Detected   := Cmd.Error_Detected;
      end return;
   end Copy_Command_Line;

   function Text_To_Switch
     (Descriptor : Command_Line_Descriptor; Text : String) return All_Switches;
   function Text_To_Switch
     (Descriptor : Command_Line_Descriptor; Text : String) return All_Switches
   is
      DT     : constant String := Dashes (Text);
      Result : All_Switches    := All_Switches'Last;
   --  This is used in case of a String_Switch or String_Seq_Switch that
   --  does not have '=' syntax. We want to choose the longest one.
   --  All_Switches'Last indicates this hasn't been set.
      begin
      for Switch in Descriptor.Allowed_Switches'Range loop
         if Enabled (Descriptor, Switch) then
            declare
               Desc : Switch_Descriptor renames
                 Descriptor.Allowed_Switches (Switch);
            begin
               if DT = Desc.Text.all then
                  if not
                    (Desc.Kind in String_Switch | String_Seq_Switch
                     and then Syntax (Descriptor, Switch) = '!')
                  then
                     return Switch;
                  end if;

               elsif Has_Prefix (DT, Prefix => Desc.Text.all)
                 and then Desc.Kind in String_Switch | String_Seq_Switch
               then
                  case Syntax (Descriptor, Switch) is
                     when '=' =>
                        pragma Assert (DT'Length > Desc.Text'Length);
                        --  Otherwise it would have been equal, above

                        if DT (Desc.Text'Length + 1) = '=' then
                           return Switch;
                        end if;

                     when ':' | '!' | '?' =>
                        if Result = All_Switches'Last
                          or else Desc.Text'Length >
                            Descriptor.Allowed_Switches (Result).Text'Length
                        then
                           Result := Switch;
                        end if;
                  end case;
               end if;
            end;
         end if;
      end loop;

      if Result = All_Switches'Last then
         Raise_Cmd_Error ("invalid switch : " & Text);
      else
         return Result;
      end if;
   end Text_To_Switch;

   procedure Parse_Helper
     (Text_Args          : Argument_List_Access; Cmd : in out Command_Line;
      Phase              : Parse_Phase; Callback : Parse_Callback;
      Collect_File_Names : Boolean; Ignore_Errors : Boolean);
   --  This does the actual parsing work, after Parse has initialized things.

   procedure Parse_Helper
     (Text_Args          : Argument_List_Access; Cmd : in out Command_Line;
      Phase              : Parse_Phase; Callback : Parse_Callback;
      Collect_File_Names : Boolean; Ignore_Errors : Boolean)
   is
      Cur : Positive := 1;
      --  Points to current element of Text_Args

      procedure Bump;
      --  Move to next element of Text_Args

      procedure Bump is
      begin
         Cur                  := Cur + 1;
         Cmd.Current_Position := Cmd.Current_Position + 1;
      end Bump;

      procedure Parse_One_Switch;
      --  Parse one element, plus the next one in case that's the parameter of
      --  the current switch (as in "--switch arg").

      procedure Parse_One_Switch is
         Text : String renames Text_Args (Cur).all;
         pragma Assert (Text'First = 1);
         Descriptor : Command_Line_Descriptor renames Cmd.Descriptor.all;
         Switch : constant All_Switches := Text_To_Switch (Descriptor, Text);
         Allowed    : String renames
           Descriptor.Allowed_Switches (Switch).Text.all;
         pragma Assert (Allowed'First = 1);
         Alias : constant All_Switches :=
           Descriptor.Allowed_Switches (Switch).Alias;
         pragma Assert (Descriptor.Allowed_Switches (Alias).Enabled);
         Dyn : Dynamically_Typed_Switch renames Cmd.Sw (Alias);
      begin
         Dyn.Text     := Descriptor.Allowed_Switches (Switch).Text;
         Dyn.Explicit := True;

         case Descriptor.Allowed_Switches (Switch).Kind is
            when No_Such =>
               raise Program_Error;

            when True_Switch =>
               Dyn.Boolean_Val := True;

            when False_Switch =>
               Dyn.Boolean_Val := False;

            when Enum_Switch =>
               Dyn.Position := Cmd.Current_Position + 1;

            when String_Switch | String_Seq_Switch =>
               --  Note that we allow the switch arg to be empty here, as in
               --  "output-dir=".
                  declare
                  First : Positive;
               begin
                  --  The case of:
                     --
                     --     command --switch arg
                     --
                     --  or:
                     --
                     --     command --switch
                     --     (where the syntax is '?' and the arg is defaulted).

                  if Text = Allowed then
                     if Syntax (Descriptor, Switch) = '?' then
                        goto Use_Default;
                     end if;

                     Bump;
                     First := 1;

                     if Cur > Text_Args'Last then
                        Raise_Cmd_Error
                          ("missing switch parameter for: " & Text);
                     end if;

                     --  The case of:
                     --
                     --     command --switch=arg
                     --
                     --  or:
                     --
                     --     command --switcharg
                     --
                     --  Split out the "arg" part.

                  else
                     pragma Assert
                       (Has_Prefix
                          (Replace_String (Text, "_", "-"),
                           Prefix => Replace_String (Allowed, "_", "-")));

                     if Text (Allowed'Length + 1) = '=' then
                        First := Allowed'Length + 2;
                     else
                        First := Allowed'Length + 1;
                     end if;
                  end if;

                  declare
                     Arg : String renames
                       Text_Args (Cur) (First .. Text_Args (Cur)'Last);
                     S : constant String (1 .. Arg'Length) := Arg;
                  --  Slide it to start at 1
                     begin
                     case Descriptor.Allowed_Switches (Switch).Kind is
                        when String_Switch =>
                           Descriptor.Allowed_Switches
                             (Descriptor.Allowed_Switches (Switch).Alias)
                             .Validator
                             (S);
                           --  Call the Validator. This will call
                           --  Raise_Cmd_Error if the switch parameter
                           --  is malformed; Raise_Cmd_Error raises.

                           if Dyn.String_Val = null
                             or else Dyn.String_Val.all /= S
                           then
                              Free (Dyn.String_Val);
                              Dyn.String_Val := new String'(S);
                           end if;
                        when String_Seq_Switch =>
                           --  If the switch appears on the command line, we
                           --  don't want to duplicate it, so we skip the
                           --  Append in the Cmd_Line_2 phase. Currently, that
                           --  works for Debug, because order doesn't
                           --  matter. It works for Files, which comes from the
                           --  command line OR the project file. It works for
                           --  External_Variable, because those aren't allowed
                           --  in a project file. If there are any
                           --  String_Seq_Switches that should come from both,
                           --  and the command-line ones should come later,
                           --  then this needs to be adjusted.

                           case Phase is
                              when Cmd_Line_1 | Project_File =>
                                 Append (Dyn.Seq_Val, new String'(S));
                              when Cmd_Line_2 =>
                                 null;
                           end case;
                        when others =>
                           raise Program_Error;
                     end case;
                  end;

                  <<Use_Default>>
               end;
         end case;

         if Callback /= null then
            Callback (Phase, Dyn);
         end if;
      end Parse_One_Switch;

   begin
      while Cur <= Text_Args'Last loop
         if Text_Args (Cur) (1) = '-' then -- Is it a switch?
            begin
               Parse_One_Switch;
            exception
               when X : Command_Line_Error =>
                  Cmd.Error_Detected := True;
                  --  This works, event though we're about to raise, because
                  --  Cmd is a pointer.

                  if not Ignore_Errors then
                     Put_Line
                       (Standard_Error,
                        Utils.Tool_Names.Tool_Name & ": " &
                        Exceptions.Exception_Message (X));
                     raise;
                  end if;
            end;

         elsif Collect_File_Names then
            declare
               --  We expand wildcards here. This is necessary on Windows, and
               --  harmless elsewhere. But we don't want to expand wildcards
               --  unless the argument actually contains wildcard characters,
               --  because we might be in the wrong directory (sources are
               --  interpreted w.r.t. project file directory), or the argument
               --  might not refer to file(s) (e.g. "-U main" where the file
               --  is main.adb).

               Has_Wildcards : constant Boolean :=
                 (for some C of Text_Args (Cur).all => C in '*' | '?' | '[');

               use GNAT.Command_Line;
               It : Expansion_Iterator;
            begin
               if Has_Wildcards then
                  Start_Expansion (It, Text_Args (Cur).all);
                  loop
                     declare
                        X : constant String := Expansion (It);
                     begin
                        exit when X = "";
                        Append (Cmd.File_Names, new String'(X));
                     end;
                  end loop;

                  --  No wildcards:

               else
                  Append (Cmd.File_Names, String_Ref (Text_Args (Cur)));
               end if;
            end;
         end if;

         Bump;
      end loop;
   end Parse_Helper;

   procedure Parse
     (Text_Args          : Argument_List_Access; Cmd : in out Command_Line;
      Phase              : Parse_Phase; Callback : Parse_Callback;
      Collect_File_Names : Boolean; Ignore_Errors : Boolean := False)
   is
      Descriptor : Command_Line_Descriptor renames Cmd.Descriptor.all;
   begin
      pragma Assert (Descriptor.Allowed_Switches /= null);
      --  Assert that Freeze_Descriptor has been called

      --  First time Parse is called for this particular Cmd object. Fill
      --  in the default values for all allowed switches.

      if Cmd.Sw = null then
         Cmd.Sw :=
           new Dynamically_Typed_Switches (Descriptor.Allowed_Switches'Range);

         for Switch in Descriptor.Allowed_Switches'Range loop
            --  Skip shorthands and negated switches

            if Descriptor.Allowed_Switches (Switch).Alias = Switch then
               case Descriptor.Allowed_Switches (Switch).Kind is
                  when No_Such | False_Switch =>
                     raise Program_Error;

                  when True_Switch =>
                     Cmd.Sw (Switch) :=
                       (True_Switch, Switch, Text => null, Explicit => False,
                        Boolean_Val               =>
                          Descriptor.Allowed_Switches (Switch).Default_Bool);

                  when Enum_Switch =>
                     Cmd.Sw (Switch) :=
                       (Enum_Switch, Switch, Text => null, Explicit => False,
                        Position                  => 0);

                  when String_Switch =>
                     Cmd.Sw (Switch) :=
                       (String_Switch, Switch, Text => null, Explicit => False,
                        String_Val                  =>
                          Descriptor.Allowed_Switches (Switch).Default);

                  when String_Seq_Switch =>
                     Cmd.Sw (Switch) :=
                       (String_Seq_Switch, Switch, Text => null,
                        Explicit                        => False,
                        Seq_Val => String_Ref_Vectors.Empty_Vector);
               end case;
            end if;
         end loop;
      end if;

      --  Finally, parse the Text_Args into Cmd

      Parse_Helper
        (Text_Args, Cmd, Phase, Callback, Collect_File_Names, Ignore_Errors);
   end Parse;

   function File_Name_Is_Less_Than (Left, Right : String_Ref) return Boolean;
   --  Assuming that L and R are file names compares them as follows:
   --
   --  * if L and/or R contains a directory separator, compares
   --    lexicographicaly parts that follow the rightmost directory separator.
   --    If these parts are equal, compares L and R lexicographicaly
   --
   --  * otherwise compares L and R lexicographicaly
   --
   --  Comparisons are case-sensitive.

   package Sorting is new String_Ref_Vectors.Generic_Sorting
     (File_Name_Is_Less_Than);

   ----------------------------
   -- File_Name_Is_Less_Than --
   ----------------------------

   function File_Name_Is_Less_Than (Left, Right : String_Ref) return Boolean is
      L : String renames Left.all;
      R : String renames Right.all;

      L_Last : constant Natural := L'Last;
      R_Last : constant Natural := R'Last;

      L_Dir_Separator : Natural :=
        Index (L, (1 => Directory_Separator), Backward);

      R_Dir_Separator : Natural :=
        Index (R, (1 => Directory_Separator), Backward);

   begin
      if L_Dir_Separator = 0 and then R_Dir_Separator = 0 then
         return L < R;
      end if;

      if L_Dir_Separator = 0 then
         L_Dir_Separator := L'First;
      end if;

      if R_Dir_Separator = 0 then
         R_Dir_Separator := R'First;
      end if;

      if L (L_Dir_Separator .. L_Last) = R (R_Dir_Separator .. R_Last) then
         return L < R;
      else
         return L (L_Dir_Separator .. L_Last) < R (R_Dir_Separator .. R_Last);
      end if;
   end File_Name_Is_Less_Than;

   procedure Sort_File_Names (Cmd : in out Command_Line) is
   begin
      Sorting.Sort (Cmd.File_Names);
   end Sort_File_Names;

   procedure Clear_File_Names (Cmd : in out Command_Line) is
   begin
      Clear (Cmd.File_Names);
   end Clear_File_Names;

   procedure Append_File_Name (Cmd : in out Command_Line; Name : String) is
      Slide : constant String (1 .. Name'Length) := Name;
   begin
      Append (Cmd.File_Names, new String'(Slide));
   end Append_File_Name;

   function File_Names (Cmd : Command_Line) return String_Ref_Array is
   begin
      return To_Array (Cmd.File_Names);
   end File_Names;

   function Num_File_Names (Cmd : Command_Line) return Natural is
   begin
      return Last_Index (Cmd.File_Names);
   end Num_File_Names;

   procedure Iter_File_Names
     (Cmd    : in out Command_Line;
      Action :    not null access procedure (File_Name : in out String_Ref))
   is
   begin
      for File_Name of Cmd.File_Names loop
         Action (File_Name);
      end loop;
   end Iter_File_Names;

   function Switch_Text
     (Descriptor : Command_Line_Descriptor; Switch : All_Switches)
      return String_Ref
   is
   begin
      return Descriptor.Allowed_Switches (Switch).Text;
   end Switch_Text;

   procedure Dump_Cmd (Cmd : Command_Line; Verbose : Boolean := False) is
      Descriptor : Command_Line_Descriptor renames Cmd.Descriptor.all;
      Sw         : Dynamically_Typed_Switches renames Cmd.Sw.all;
   begin
      if Is_Empty (Cmd.File_Names) then
         Put_Line ("No file names");
      else
         Put ("file names:");

         for Name of Cmd.File_Names loop
            Put (" " & Name.all);
         end loop;

         Put_Line ("");
      end if;

      for Switch in Sw'Range loop
         --  Skip shorthands and negated switches

         if Descriptor.Allowed_Switches (Switch).Alias = Switch then
            --  If not Verbose, skip defaulted switches

            if not Verbose then
               case Sw (Switch).Kind is
                  when No_Such | False_Switch =>
                     raise Program_Error;
                  when True_Switch =>
                     if Sw (Switch).Boolean_Val =
                       Descriptor.Allowed_Switches (Switch).Default_Bool
                     then
                        goto Continue;
                     end if;
                  when Enum_Switch =>
                     null; -- Too much trouble to determine default here
                  when String_Switch =>
                     if Sw (Switch).String_Val =
                       Descriptor.Allowed_Switches (Switch).Default
                     then
                        goto Continue;
                     end if;
                     if Descriptor.Allowed_Switches (Switch).Default /= null
                       and then Sw (Switch).String_Val.all =
                         Descriptor.Allowed_Switches (Switch).Default.all
                     then
                        goto Continue;
                     end if;
                  when String_Seq_Switch =>
                     if Last_Index (Sw (Switch).Seq_Val) = 0 then
                        goto Continue;
                     end if;
               end case;
            end if;

            Put
              (Descriptor.Allowed_Switches (Switch).Text.all & ": " &
               Sw (Switch).Kind'Img & " := ");

            case Sw (Switch).Kind is
               when No_Such | False_Switch =>
                  raise Program_Error;
               when True_Switch =>
                  Put (if Sw (Switch).Boolean_Val then "True" else "False");
               when Enum_Switch =>
                  Put ("at" & Sw (Switch).Position'Img);
               when String_Switch =>
                  if Sw (Switch).String_Val = null then
                     Put ("null");
                  else
                     Put ("""" & Sw (Switch).String_Val.all & """");
                  end if;
               when String_Seq_Switch =>
                  Put ("(");
                  declare
                     First_Time : Boolean := True;
                  begin
                     for S of To_Array (Sw (Switch).Seq_Val) loop
                        if First_Time then
                           First_Time := False;
                        else
                           Put (", ");
                        end if;
                        Put ("""" & S.all & """");
                     end loop;
                  end;
                  Put (")");
            end case;

            Put_Line ("");
         end if;
         <<Continue>>
      end loop;
   end Dump_Cmd;

   procedure Dump_Descriptor (Descriptor : Command_Line_Descriptor) is
      pragma Assert (Descriptor.Allowed_Switches /= null);
      pragma Assert (Is_Empty (Descriptor.Allowed_Switches_Vector));
   --  Assert that Freeze_Descriptor has been called
      begin
      for J in Descriptor.Allowed_Switches'Range loop
         declare
            X : Switch_Descriptor renames Descriptor.Allowed_Switches (J);
            S : constant String :=
              (if X.Kind in String_Switch | String_Seq_Switch then
                 Syntax (Descriptor, J)'Img
               else "");
         begin
            Put_Line
              (J'Img & " " & X.Kind'Img & S & " " & X.Text.all &
               (if X.Alias = J then "" else X.Alias'Img) & " " &
               (if X.Enabled then "" else " DISABLED"));
         end;
      end loop;
   end Dump_Descriptor;

end Utils.Command_Lines;
