with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
package Test.Command_Lines is

   package Test_Common_Nat_Shorthands is new Common_Nat_Switches
     .Set_Shorthands
       ((Jobs => +"--queues"));

   package Freeze_Common is new Freeze_Descriptor (Common_Descriptor);

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   type Test_Booleans is
     (Strict,
      Recursive,
      Harness_Only,
      Stub,
      Validate_Type_Extensions,
      Inheritance_Check,
      Test_Case_Only,
      Omit_Sloc,
      Command_Line_Support,
      Separates,
      Transition,
      Test_Duration);

   package Test_Boolean_Switches is new Boolean_Switches
     (Descriptor,
      Test_Booleans);

   package Test_Boolean_Shorthands is new Test_Boolean_Switches
     .Set_Shorthands
     ((Recursive => +"-r",
       Command_Line_Support => +"--command-line",
       others => null));

   --  Re: the --command-line/--no-command-line switch. We don't want an
   --  enumeration literal Command_Line here, because it causes conflicts
   --  with the type of the same name. So we call it Command_Line_Support,
   --  and add --command-line as a shorthand.

   package Test_Boolean_Defaults is new
     Test_Boolean_Switches.Set_Defaults
       ((Inheritance_Check => False, -- ????Not sure about this default
         Command_Line_Support => True,
         others => False));

   type Test_Strings is
     (Separate_Drivers,
      Harness_Dir,
      Tests_Dir,
      Subdir,
      Tests_Root,
      Stubs_Dir,
      Exclude_From_Stubbing,
      --  ????Not sure about --exclude-from-stubbing:{unit}={filename}
      Skeleton_Default,
      Passed_Tests,
      Exit_Status,
      Copy_Environment);

   package Test_String_Switches is new String_Switches
     (Descriptor,
      Test_Strings);

   --  ????Perhaps following should use Other_Switches, with an enum type
   --  for each one (e.g. (Unit, Test) or (Off, On), etc.
   package Test_String_Defaults is new Test_String_Switches.Set_Defaults
     ((Separate_Drivers => +"unit", -- or "test"
       Skeleton_Default => +"fail", -- or "pass"
       Passed_Tests => +"show", -- or "hide"
       Exit_Status => +"off", -- or "on"
       others => null));

   type Test_String_Seqs is (Additional_Tests);

   package Test_String_Seq_Switches is new String_Seq_Switches
     (Descriptor,
      Test_String_Seqs);

   package Test_String_Seq_Syntax is new Test_String_Seq_Switches
     .Set_Syntax
     ((Additional_Tests => '='));

   package Freeze is new Freeze_Descriptor (Descriptor);

end Test.Command_Lines;
