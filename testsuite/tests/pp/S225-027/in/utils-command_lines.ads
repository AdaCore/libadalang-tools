with GNAT.OS_Lib; use GNAT.OS_Lib;

with Utils.Vectors;
with Ada.Containers.Hashed_Sets; use Ada.Containers;
with GNAT.String_Hash;

package Utils.Command_Lines is

   --  Processing of command-line arguments (including arguments coming from
   --  a project file, which aren't really "command line", but need to be
   --  processed in more-or-less the same way).

   --  Step 1. Define the switches to be processed by instantiating the
   --  generic packages below, normally at library level. A tool will typically
   --  instantiate the _Switches generics many times, and then optionally
   --  instantiate the generics within the _Switches generics.
   --
   --  Each _Switches generic takes an enumeration type Switches as a
   --  parameter; this defines the names of the switches. The switch name is
   --  the enumeral, converted to lower case, with "_" changed to "-", and
   --  "--" prepended. For example:
   --
   --     type File_Options is (Replace, Replace_Force, Replace_No_Backup);
   --     package File_Options_Switches is new Enum_Switches (File_Options);
   --     use File_Options_Switches; -- you normally want 'use' on all these
   --
   --  If we pass Switches => File_Options, that defines switches:
   --
   --     --replace, --replace-force, --replace-no-backup
   --
   --  Underscores are also accepted except at the beginning, so
   --  --replace_no_backup means the same as --replace-no-backup.
   --
   --  You can then use Set_Shorthands to add shorthands:
   --
   --     package File_Options_Shorthands is new Set_Shorthands
   --       ((Replace => +"-r",
   --         Replace_Force => +"-rf",
   --         Replace_No_Backup => +"-rnb"));

   --  Freeze_Descriptor must be "called" after Step 1 and before Step 2.
   --  Instantiated, really.

   --  Step 2. Parse the command line one or more times. Parse takes a sequence
   --  of strings (Argument_List_Access) as input. This can come from the
   --  command line (see Text_Args_From_Command_Line), or from a project file,
   --  or from any other source you like. The output is of type Command_Line.
   --  Example:
   --
   --     Cmd : Command_Line;
   --     Parse (Text_Args_From_Command_Line, Cmd);

   --  Step 3. Query the Command_Line by calling the Arg functions below.
   --
   --     X : constant File_Options := Arg (Cmd);
   --     case X is
   --        when Replace =>
   --        ...

   --  So Step 1 is very static/declarative. It's basically declaring the names
   --  and types of the switches. It serves the same purpose for a program
   --  that a formal_part serves for a procedure. Step 2, on the other hand,
   --  is where we look at the actual command-line typed by the user.

   --  See Utils.Command_Lines.Common for an example of Step 1, and see
   --  Utils.Projects for an example of Step 2.

   type Command_Line_Descriptor is limited private;
   --  This is produced by Step 1, and doesn't depend on the actual command
   --  line text or any other inputs. It contains information about what
   --  switches are allowed, what is their type, and so on.

   type Descriptor_Access is access all Command_Line_Descriptor;

   generic
      Descriptor : in out Command_Line_Descriptor;
   package Freeze_Descriptor is
   --  Freezes the Descriptor; no more switches may be added. Freeze_Descriptor
   --  must be called before calling Parse or Copy_Descriptor.
   end Freeze_Descriptor;

   function Copy_Descriptor
     (Descriptor : Command_Line_Descriptor) return Command_Line_Descriptor;
   --  Returns a copy of Descriptor. This allows you to create a common/shared
   --  descriptor, and copy it to a descriptor for a particular tool, before
   --  adding tool-specific switches. Descriptor must be frozen; the result
   --  is not.

   type Command_Line (Descriptor : Descriptor_Access) is limited private;
   --  Result of Parse (Step 2). Depends on the command line typed by the user.

   function Copy_Command_Line (Cmd : Command_Line) return Command_Line;

   --  Warn about mixing the wrong Command_Line????

   type All_Switches is private;
   --  This is conceptually an enumeration type that is the union of all the
   --  Switches enumeration types passed when instantiating the _Switches
   --  generics. The To_All functions below convert from particular Switches
   --  types to this one, producing a unique value across all the instances
   --  (within a given instance of Command_Line_Descriptor).
   --
   --  From_All functions do the opposite conversion. Valid functions return
   --  True if the All_Switches value comes from that particular instance.
   --
   --  So typical usage would be:
   --     if Some_Instance.Valid (X) then
   --        case Some_Enum'(From_All (X)) is
   --           ...
   --        end case;
   --     elsif Other_Instance.Valid (X) then
   --        ...

   type Switch_Syntax is (':', '=', '!', '?');
   --   ':'  The switch requires a parameter. There can optionally be a space
   --        on the command line between the switch and its parameter.
   --
   --   '='  The switch requires a parameter. There can either be a '=' or a
   --        space on the command line between the switch and its parameter.
   --
   --   '!'  The switch requires a parameter, but there can be no space on the
   --        command line between the switch and its parameter.
   --
   --   '?'  The switch may have an optional parameter. There can be no space
   --        between the switch and its argument.
   --
   --  We actually don't distinguish between ':', '=', and '!'.

   type Validator_Type is not null access procedure (Text : String);
   --  For internal use only

   subtype String_Ref is GNAT.OS_Lib.String_Access with
       Predicate => (if String_Ref /= null then String_Ref'First = 1);
   type String_Ref_Array is array (Positive range <>) of String_Ref;

   function Present (X : String_Ref) return Boolean is
     (GNAT.OS_Lib."/=" (X, null));

   package String_Ref_Vectors is new Utils.Vectors (Positive, String_Ref,
      String_Ref_Array);
   use String_Ref_Vectors;
   subtype String_Ref_Vector is String_Ref_Vectors.Vector;

   function Hash_String is new GNAT.String_Hash.Hash (Character, String,
      Hash_Type);
   function Hash_String_Ref (X : String_Ref) return Hash_Type is
     (Hash_String (X.all));
   function String_Eq (X, Y : String_Ref) return Boolean is (X.all = Y.all);

   package String_Ref_Sets is new Ada.Containers.Hashed_Sets (String_Ref,
      Hash_String_Ref, String_Eq, String_Eq);
   subtype String_Ref_Set is String_Ref_Sets.Set;

   --  ???For each _Switches generic, specify:
   --  Help text. Allowed in proj file. Or specify
   --  on Parse?

   generic
      Descriptor : in out Command_Line_Descriptor;
      type Switches is (<>);
   package Flag_Switches is

      --  An instance of this generic declares one or more Flag switches, all
      --  independent of one another. Each switch can appear on the command
      --  line, or not.

      function To_All (Switch : Switches) return All_Switches;
      function From_All (Switch : All_Switches) return Switches;
      function Valid (Switch : All_Switches) return Boolean;

      function Arg (Cmd : Command_Line; Switch : Switches) return Boolean;
      --  Arg (Cmd, Verbose), where Verbose is one enumeral in
      --  Switches, returns True if "--verbose" appeared on the command
      --  line (or "-v" appeared, if that is a shorthand); False otherwise.

      procedure Set_Arg
        (Cmd : in out Command_Line; Switch : Switches; Val : Boolean := True);
      --  Set the given switch to True.

      type Switch_To_String_Mapping is array (Switches) of String_Ref;

      generic
         Shorthands : Switch_To_String_Mapping;
      package Set_Shorthands is

      --  This is really a procedure, but we make it a generic so we can
      --  "call" it (by instantiating it) in a package spec. All the work is
      --  done by the elaboration code in the body. The same is true of
      --  other empty generics below.

      --  Instantiate this with an aggregate that maps Switches to strings
      --  that should be used as shorthands, for example Verbose => +"-v"
      --  will allow either "--verbose" or "-v" on the command line.
      --  (The "+" function is declared below; it just converts String to
      --  String_Ref.)

      --  You can instantiate this multiple times to get multiple
      --  shorthands. Use null for Switches that you don't want a
      --  shorthand for.

      end Set_Shorthands;

   end Flag_Switches;

   generic
      Descriptor : in out Command_Line_Descriptor;
      type Switches is (<>);
   package Boolean_Switches is

      --  An instance of this generic declares one or more Boolean switches,
      --  all independent of one another. If Switches has an enumeral
      --  Syntax_Check, then --syntax-check turns the switch ON, and
      --  --no-syntax-check turns it OFF. The default can be set with
      --  Set_Defaults; the default for the default is False. The default for a
      --  shorthand is taken from the main switch. If the same switch is given
      --  more than once, the last one wins.

      function To_All (Switch : Switches) return All_Switches;
      function From_All (Switch : All_Switches) return Switches;
      function Valid (Switch : All_Switches) return Boolean;

      function Arg (Cmd : Command_Line; Switch : Switches) return Boolean;

      procedure Set_Arg
        (Cmd : in out Command_Line; Switch : Switches; Val : Boolean := True);
      --  Set the given switch to Val.

      function Explicit (Cmd : Command_Line; Switch : Switches) return Boolean;
      --  True if the switch was specified explicitly, as opposed to being
      --  defaulted.

      type Switch_To_Boolean_Mapping is array (Switches) of Boolean;
      type Switch_To_String_Mapping is array (Switches) of String_Ref;

      generic
         Defaults : Switch_To_Boolean_Mapping;
      package Set_Defaults is
      end Set_Defaults;

      generic
         Shorthands : Switch_To_String_Mapping;
      package Set_Shorthands is
      end Set_Shorthands;

   end Boolean_Switches;

   generic
      Descriptor : in out Command_Line_Descriptor;
      type Switches is (<>);
      Default : Switches := Switches'First;
   package Enum_Switches is

      --  An instance of this generic declares an enumeration-like set of
      --  mutually-exclusive switches. The last one that is Parsed is the one
      --  that takes effect. So if Switches is (This, That), and the command
      --  line says "--that --other --this --whatever --that", then Arg
      --  will return That. The default is Switches'First, so if neither
      --  "--this" nor "--that" is given, Arg will return This.

      function To_All (Switch : Switches) return All_Switches;
      function From_All (Switch : All_Switches) return Switches;
      function Valid (Switch : All_Switches) return Boolean;

      function Arg (Cmd : Command_Line) return Switches;
      --  Return the value corresponding to the last one Parsed

      procedure Set_Arg (Cmd : in out Command_Line; Switch : Switches);
      --  Set the given switch.

      type Switch_To_String_Mapping is array (Switches) of String_Ref;

      generic
         Shorthands : Switch_To_String_Mapping;
      package Set_Shorthands is
      --  As above
      end Set_Shorthands;

   end Enum_Switches;

   generic
      Descriptor : in out Command_Line_Descriptor;
      type Switches is (<>);
   package String_Switches is

      --  An instance of this generic declares one or more switches, all
      --  independent of one another, that can take a "parameter". For
      --  example, if Switches has an enumeral Output_Dir, we can have
      --  "--output-dir=/some/directory", where "--output-dir" is the switch
      --  name, and "/some/directory" is its parameter. If the same switch is
      --  given more than once, the last one wins.

      --  Depending on the syntax, it could be one of:
      --    --output-dir=/some/directory
      --    --output-dir /some/directory
      --    --output-dir/some/directory
      --  with the switch and parameter separated by "=", space, or nothing.

      function To_All (Switch : Switches) return All_Switches;
      function From_All (Switch : All_Switches) return Switches;
      function Valid (Switch : All_Switches) return Boolean;

      function Arg (Cmd : Command_Line; Switch : Switches) return String_Ref;
      --  Return the parameter of the given switch. If the switch was not
      --  specified, then null is returned, unless a default is set up
      --  (see below).

      procedure Set_Arg
        (Cmd : in out Command_Line; Switch : Switches; Val : String);
      --  Set parameter of the given switch.

      type Switch_To_Syntax_Mapping is array (Switches) of Switch_Syntax;
      type Switch_To_String_Mapping is array (Switches) of String_Ref;

      generic
         Syntax : Switch_To_Syntax_Mapping;
      package Set_Syntax is
      --  The syntax of a switch is '=' by default. Instantiate this to set
      --  the Syntax of each switch.
      end Set_Syntax;

      generic
         Defaults : Switch_To_String_Mapping;
      package Set_Defaults is
      --  Instantiate this to set the default value for a switch
      --  parameter. If this is not instantiated, then if the switch is not
      --  given, Arg returns null.
      end Set_Defaults;

      --  Set_Syntax and Set_Defaults should be instantiated before
      --  Set_Shorthands (if both are instantiated).

      generic
         Shorthands : Switch_To_String_Mapping;
      package Set_Shorthands is
      --  As above
      end Set_Shorthands;

   end String_Switches;

   generic
      Descriptor : in out Command_Line_Descriptor;
      type Switches is (<>);
   package String_Seq_Switches is

      --  This is the same as String_Switches, except that the last one
      --  doesn't win. Instead, all of them are returned from Arg as
      --  a String_Ref_Array. The default is the empty sequence.

      function To_All (Switch : Switches) return All_Switches;
      function From_All (Switch : All_Switches) return Switches;
      function Valid (Switch : All_Switches) return Boolean;

      function Arg
        (Cmd : Command_Line; Switch : Switches) return String_Ref_Array;
      function Arg_Length
        (Cmd : Command_Line; Switch : Switches) return Natural with
        Post => Arg_Length'Result = Arg (Cmd, Switch)'Length;

      procedure Set_Arg
        (Cmd : in out Command_Line; Switch : Switches; Val : String_Ref_Array);
      --  Set parameter of the given switch.

      type Switch_To_Syntax_Mapping is array (Switches) of Switch_Syntax;
      type Switch_To_String_Mapping is array (Switches) of String_Ref;

      generic
         Syntax : Switch_To_Syntax_Mapping;
      package Set_Syntax is
      --  As above
      end Set_Syntax;

      generic
         Shorthands : Switch_To_String_Mapping;
      package Set_Shorthands is
      --  As above
      end Set_Shorthands;

   end String_Seq_Switches;

   generic
      Descriptor : in out Command_Line_Descriptor;
      type Switches is (<>);
      type Arg_Type is private;
      with function Image (Arg : Arg_Type) return String is <>;
      with function Value (Text : String) return Arg_Type is <>;
   --  Value should raise Constraint_Error if Text is malformed. We choose
   --  Constraint_Error because that allows Arg_Type'Value to be used when
   --  available.
      package Other_Switches is

      --  This is similar to String_Switches, except that the parameter of a
      --  switch can be of any type (Arg_Type). You specify conversions to/from
      --  String. For example, "--jobs=12" where jobs is of subtype Natural.
      --  Pass Arg_Type => Natural, and Natural'Image and Natural'Value as the
      --  conversions.

      function To_All (Switch : Switches) return All_Switches;
      function From_All (Switch : All_Switches) return Switches;
      function Valid (Switch : All_Switches) return Boolean;

      function Arg (Cmd : Command_Line; Switch : Switches) return Arg_Type;
      --  Same as for String_Switches, except that the string typed by the
      --  user is converted to the right type. Note that this does not raise
      --  an exception for malformed parameters; that happens earlier, when you
      --  call Parse.

      procedure Set_Arg
        (Cmd : in out Command_Line; Switch : Switches; Val : Arg_Type);
      --  Set parameter of the given switch to Val.

      type Switch_To_Syntax_Mapping is array (Switches) of Switch_Syntax;
      type Switch_To_Arg_Type_Mapping is array (Switches) of Arg_Type;

      generic
         Syntax : Switch_To_Syntax_Mapping;
      package Set_Syntax is
      --  As above
      end Set_Syntax;

      generic
         Defaults : Switch_To_Arg_Type_Mapping;
      package Set_Defaults is

      --  As for String_Switches, except that this MUST be instantiated to
      --  set Default, because otherwise Arg will blow up.

      end Set_Defaults;

      type Switch_To_String_Mapping is array (Switches) of String_Ref;

      generic
         Shorthands : Switch_To_String_Mapping;
      package Set_Shorthands is
      --  As above
      end Set_Shorthands;

   private
      procedure Validate (Text : String);
      --  Call Error if Text is a malformed string; that is, if Value (Text)
      --  raises Constraint_Error.
      Validate_Access : constant Validator_Type := Validate'Access;
   end Other_Switches;

   type Switch_Array is array (Positive range <>) of All_Switches;

   generic
      Descriptor : in out Command_Line_Descriptor;
      Disable : Switch_Array;
   package Disable_Switches is

   --  Instantiate this to disable some switches; Parse behaves as if the
   --  disabled switches did not exist. This allows us to have switches that
   --  are common to SOME tools, but not allowed for others.

   end Disable_Switches;

   ----------------------------------------------------------------

   package String_Access_Vectors is new Utils.Vectors (Positive,
      GNAT.OS_Lib.String_Access, GNAT.OS_Lib.String_List);
   use String_Access_Vectors;
   subtype String_Access_Vector is String_Access_Vectors.Vector;

   type Parse_Phase is (Cmd_Line_1, Project_File, Cmd_Line_2);
   --  When Parse is called from the project-file support code (see
   --  Utils.Projects), it is called on the command-line arguments, then on
   --  the switches from the project file (if there is one), and then on the
   --  command-line arguments again. The Parse_Phase indicates which of these
   --  three phases is the current one.

   type Switch_Kind is
     (No_Such, True_Switch, False_Switch, Enum_Switch, String_Switch,
      String_Seq_Switch);
   --  ????????????????Use Switch_Type from g-comlin.ads? Use other stuff
   --  from there? True_Switch is for both Flag_Switches and Boolean_Switches.
   --  False_Switch is for the negation of a Boolean_Switch, as in
   --  "--no-syntax-check". String_Switch is for both String_Switches
   --  and Other_Switches.

   type Logical_Position is new Natural;

   type Dynamically_Typed_Switch (Kind : Switch_Kind := No_Such) is record
      Switch : All_Switches;

      Text : String_Ref;
      --  The text of the switch; could be a shorthand

      Explicit : Boolean;

      case Kind is
         when No_Such | False_Switch =>
            null; -- Can't happen
         when True_Switch =>
            Boolean_Val : Boolean;
         --  For Flag_Switches, True if the switch was given. For
         --  Boolean_Switches, True if the switch was given, False if
         --  its negation was given (last one wins).
            when Enum_Switch =>
            Position : Logical_Position;
         --  If the switch was given, this is its index in the sequence of
         --  switches processed. If Parse is called multiple times, the current
         --  position keeps increasing. Within a given Enum switch, the one
         --  with the last position wins.
            when String_Switch =>
            String_Val : String_Ref;
         when String_Seq_Switch =>
            Seq_Val : String_Ref_Vector;
      end case;
   end record; -- Dynamically_Typed_Switch

   type Parse_Callback is access procedure
       (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch);

   procedure Null_Callback
     (Phase : Parse_Phase; Swit : Dynamically_Typed_Switch) is null;

   procedure Parse
     (Text_Args          : Argument_List_Access; Cmd : in out Command_Line;
      Phase              : Parse_Phase; Callback : Parse_Callback;
      Collect_File_Names : Boolean; Ignore_Errors : Boolean := False);
   --  Given Text_Args (from the command line, project file, or elsewhere),
   --  parse that information into Cmd. This may be called multiple
   --  times, with later information overriding earlier.
   --
   --  If Collect_File_Names is True, we record the file-name args in Cmd.
   --  Otherwise, we ignore them, because presumably they were already recorded
   --  on a previous call to Parse.
   --
   --  Ignore_Errors is normally set True the first time, so we can detect
   --  switches like "--help" and "--version", ignoring all other switches. If
   --  Ignore_Errors is False, an error causes a message to be printed, and
   --  Command_Line_Error to be raised. The caller should print the "try
   --  --help" message and exit the program.

   procedure Clear_File_Names (Cmd : in out Command_Line);
   --  Sets the File_Names of Cmd to empty

   procedure Sort_File_Names (Cmd : in out Command_Line);
   --  The names are sorted to provide a predictable order.

   procedure Append_File_Name (Cmd : in out Command_Line; Name : String);
   --  Appends Name onto the File_Names of Cmd. Names on the command line
   --  are appended by Parse. This is used for file names from the project
   --  file, and for file names read from a file (the -files=par_file_name
   --  switch).

   function File_Names (Cmd : Command_Line) return String_Ref_Array;
   --  Returns the sequence of non-switch arguments. They're not necessarily
   --  file names, but they are in most cases, so we call it that.

   function Num_File_Names (Cmd : Command_Line) return Natural;

   procedure Iter_File_Names
     (Cmd    : in out Command_Line;
      Action :    not null access procedure (File_Name : in out String_Ref));
   --  Calls Action for each non-switch argument. Note that File_Name is
   --  'in out'; the caller can modify the file name. This is necessary for
   --  Process_Project. The names are sorted to provide a predictable order.

   function Switch_Text
     (Descriptor : Command_Line_Descriptor; Switch : All_Switches)
      return String_Ref;
   --  Switch_Text (To_All (Some_Switch)) --> "--some-switch"

   procedure Dump_Cmd (Cmd : Command_Line; Verbose : Boolean := False);
   --  Debugging printout. Without Verbose, skips defaulted args.

   procedure Dump_Descriptor (Descriptor : Command_Line_Descriptor);

   function Text_Args_From_Command_Line return Argument_List_Access;
   --  Returns the sequence of command-line arguments

   function Text_Cargs_From_Command_Line return Argument_List_Access;
   --  Returns the -cargs sections

   function "+" (S : String) return String_Ref is (new String'(S));
   --  Hack to get around the fact that Ada doesn't allow arrays of String

   Command_Line_Error, Command_Line_Error_No_Help,
   Command_Line_Error_No_Tool_Name : exception;
   --  Raised by Parse if there are errors. The tool should handle these,
   --  print out the Exception_Message, and exit the process.

   function Error_Detected (Cmd : Command_Line) return Boolean;
   --  True if Parse detected an error

   procedure Cmd_Error (Message : String) with
     No_Return;
   --  Prints an error message, and raises Command_Line_Error. This is
   --  called by Parse to report errors, and may also be used by clients
   --  of Parse.

   procedure Cmd_Error_No_Help (Message : String) with
     No_Return;
   --  Same as Cmd_Error, but doesn't print the "try help" message

   procedure Cmd_Error_No_Tool_Name (Message : String) with
     No_Return;
   --  Same as Cmd_Error, but doesn't print the tool name, and raises
   --  Command_Line_Error_No_Tool_Name instead of Command_Line_Error.  It's not
   --  clear why we want to suppress the tool name, but we're mimicking the way
   --  ASIS tools work.

   procedure Raise_Cmd_Error (Message : String) with
     No_Return;
--  Raises Command_Line_Error with the given message.

private

   type All_Switches is new Positive;

   procedure Null_Validator (Text : String) is null;

   type Switch_Descriptor (Kind : Switch_Kind := No_Such) is record
      Text : String_Ref;
      --  Text of the switch. For the main switch, if the enumeral is
      --  Output_Dir, this will be "--output-dir". For a shorthand, this
      --  will be the shorthand.

      Alias : All_Switches;
      --  For the main switch this points to itself. For a shorthand,
      --  this points to the main switch. For a False_Switch (e.g.
      --  "--no-syntax-check"), this points to the corresponding
      --  True_Switch (e.g. "--syntax-check").

      Enabled : Boolean := True;

      case Kind is
         when No_Such =>
            null;
         when True_Switch =>
            Default_Bool : Boolean := False;
         when False_Switch =>
            null;
         when Enum_Switch =>
            null;
         when String_Switch | String_Seq_Switch =>
            Syntax : Switch_Syntax := '=';
            case Kind is
               when String_Switch =>
                  Default : String_Ref := null;

                  Validator : Validator_Type := Null_Validator'Access;
               --  Called during Parse to validate the form of a string switch.
               --  Currently used only for Other_Switches. This is necessary
               --  so we can give an error for malformed switches during Parse,
               --  rather than allowing some Arg query to blow up later.
                  when others =>
                  null;
            end case;
      end case;
   end record;

   type Switch_Descriptor_Array is
     array (All_Switches range <>) of Switch_Descriptor;

   package Switch_Descriptor_Vectors is new Utils.Vectors (All_Switches,
      Switch_Descriptor, Switch_Descriptor_Array);
   use Switch_Descriptor_Vectors;

   type Command_Line_Descriptor is limited record
      Allowed_Switches_Vector : Switch_Descriptor_Vectors.Vector;
      Allowed_Switches        : access Switch_Descriptor_Array := null;
   --  Allowed_Switches_Vector is built up by Step 1 (instantiations
   --  of _Switches generics). Then Step 2 (Parse) copies that into
   --  Allowed_Switches, and Allowed_Switches_Vector is no longer used.
   end record;

   function Enabled
     (Descriptor : Command_Line_Descriptor; Switch : All_Switches)
      return Boolean is
     (Descriptor.Allowed_Switches (Descriptor.Allowed_Switches (Switch).Alias)
        .Enabled);

   function Syntax
     (Descriptor : Command_Line_Descriptor; Switch : All_Switches)
      return Switch_Syntax is
     (Descriptor.Allowed_Switches (Descriptor.Allowed_Switches (Switch).Alias)
        .Syntax);

   type Dynamically_Typed_Switches is
     array (All_Switches range <>) of Dynamically_Typed_Switch;
   type Dynamically_Typed_Switches_Access is access Dynamically_Typed_Switches;

   type Command_Line (Descriptor : Descriptor_Access) is limited record
      File_Names       : String_Ref_Vector;
      Current_Position : Logical_Position                  := 0;
      Sw               : Dynamically_Typed_Switches_Access := null;
      Error_Detected   : Boolean                           := False;

      --  File_Names is the sequence of non-switch arguments. Current_Position
      --  counts the number of switches encountered. Sw is the switches.
      --  Error_Detected is True if Parse detected an error, whether or
      --  not Ignore_Errors is set.
   end record;

end Utils.Command_Lines;
