with Utils.Command_Lines;        use Utils.Command_Lines;
with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
package Stub.Command_Lines is

   package Freeze_Common is new Freeze_Descriptor (Common_Descriptor);

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   pragma Warnings (Off);
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;
   pragma Warnings (On);

   package Stub_Disable is new Disable_Switches (Descriptor,
      (To_All (Rep_Clauses), To_All (Compute_Timing)));

   type Stub_Flags is
     (Subunits, Force, Alphabetical_Order, Comment_Header_Sample,
      Comment_Header_Spec, Ignored_Keep_Tree_File, No_Exception,
      No_Local_Header, Ignored_Reuse_Tree_File, Ignored_Overwrite_Tree_File);
   --  Above "Ignored_" switches are legacy switches from the ASIS-based
   --  version.

   package Stub_Flag_Switches is new Flag_Switches (Descriptor, Stub_Flags);

   package Stub_Flag_Shorthands is new Stub_Flag_Switches.Set_Shorthands
     ((Subunits => null, Force => +"-f", Alphabetical_Order => +"-gnatyo",
       Comment_Header_Sample       => +"-hg", Comment_Header_Spec => +"-hs",
       Ignored_Keep_Tree_File      => +"-k", No_Exception => null,
       No_Local_Header             => null, Ignored_Reuse_Tree_File => +"-r",
       Ignored_Overwrite_Tree_File => +"-t"));

   type Stub_Strings is (Header_File, Output);

   package Stub_String_Switches is new String_Switches (Descriptor,
      Stub_Strings);

   package Stub_String_Syntax is new Stub_String_Switches.Set_Syntax
     ((Header_File => '=', Output => '='));

   package Stub_String_Shorthands is new Stub_String_Switches.Set_Shorthands
     ((Header_File => null, Output => +"-o"));

   --  ???Perhaps Max_Line_Length, Indentation should be moved to Common, and
   --  gnatpp and gnatstub shorthands unified. Output is also shared between
   --  gnatpp and gnatstub. Or perhaps gnatstub should import Pp.Command_Lines.

   type Stub_Nats is (Max_Line_Length, Indentation, Update_Body);
   --  Update_Body is intended mainly for use by GPS or other text editors

   package Stub_Nat_Switches is new Other_Switches (Descriptor, Stub_Nats,
      Natural, Natural'Image, Natural'Value);

   package Stub_Nat_Syntax is new Stub_Nat_Switches.Set_Syntax
     ((Max_Line_Length => '!', Indentation => '!', Update_Body => '='));

   No_Update_Body : constant Natural := 0;

   package Stub_Nat_Defaults is new Stub_Nat_Switches.Set_Defaults
     ((Max_Line_Length => 79, Indentation => 3,
       Update_Body     => No_Update_Body));

   package Stub_Nat_Shorthands is new Stub_Nat_Switches.Set_Shorthands
     ((Max_Line_Length => +"-gnatyM", Indentation => +"-gnaty",
       Update_Body     => null));

   package Stub_Nat_Shorthands_2 is new Stub_Nat_Switches.Set_Shorthands
     ((Max_Line_Length => +"-l", Indentation => +"-i", Update_Body => null));

   package Freeze is new Freeze_Descriptor (Descriptor);

   pragma Warnings (Off);
   use Stub_Flag_Switches, Stub_String_Switches, Stub_Nat_Switches;
   pragma Warnings (On);

   subtype Cmd_Line is Command_Line;

   function Update_Body_Specified (Cmd : Cmd_Line) return Boolean is
     (Arg (Cmd, Update_Body) /= No_Update_Body);
--  If --update-body was not specified on the command line, then it will be
--  equal to the default (No_Update_Body).

end Stub.Command_Lines;
