------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Utils.Symbols; use Utils.Symbols;
with Utils.String_Utilities;

package Utils.Predefined_Symbols is

   pragma Assert (Last_Symbol = No_Symbol);
   --  We want this to be elaborated before any other elaboration code that
   --  Interns symbols, so anything else that does so during elaboration should
   --  "with" this. If not, this Assert will fail.

   --  Ada 83 reserved words

   Name_Abort     : constant Symbol := Intern ("abort");
   First_Reserved_Word_Sym : constant Symbol := Last_Symbol;
   Name_Abs       : constant Symbol := Intern ("abs");
   Name_Accept    : constant Symbol := Intern ("accept");
   Name_Access    : constant Symbol := Intern ("access");
   Name_And       : constant Symbol := Intern ("and");
   Name_All       : constant Symbol := Intern ("all");
   Name_Array     : constant Symbol := Intern ("array");
   Name_At        : constant Symbol := Intern ("at");
   Name_Begin     : constant Symbol := Intern ("begin");
   Name_Body      : constant Symbol := Intern ("body");
   Name_Case      : constant Symbol := Intern ("case");
   Name_Constant  : constant Symbol := Intern ("constant");
   Name_Declare   : constant Symbol := Intern ("declare");
   Name_Delay     : constant Symbol := Intern ("delay");
   Name_Delta     : constant Symbol := Intern ("delta");
   Name_Digits    : constant Symbol := Intern ("digits");
   Name_Do        : constant Symbol := Intern ("do");
   Name_Else      : constant Symbol := Intern ("else");
   Name_Elsif     : constant Symbol := Intern ("elsif");
   Name_End       : constant Symbol := Intern ("end");
   Name_Entry     : constant Symbol := Intern ("entry");
   Name_Exception : constant Symbol := Intern ("exception");
   Name_Exit      : constant Symbol := Intern ("exit");
   Name_For       : constant Symbol := Intern ("for");
   Name_Function  : constant Symbol := Intern ("function");
   Name_Generic   : constant Symbol := Intern ("generic");
   Name_Goto      : constant Symbol := Intern ("goto");
   Name_If        : constant Symbol := Intern ("if");
   Name_In        : constant Symbol := Intern ("in");
   Name_Is        : constant Symbol := Intern ("is");
   Name_Limited   : constant Symbol := Intern ("limited");
   Name_Loop      : constant Symbol := Intern ("loop");
   Name_Mod       : constant Symbol := Intern ("mod");
   Name_New       : constant Symbol := Intern ("new");
   Name_Not       : constant Symbol := Intern ("not");
   Name_Null      : constant Symbol := Intern ("null");
   Name_Of        : constant Symbol := Intern ("of");
   Name_Or        : constant Symbol := Intern ("or");
   Name_Others    : constant Symbol := Intern ("others");
   Name_Out       : constant Symbol := Intern ("out");
   Name_Package   : constant Symbol := Intern ("package");
   Name_Pragma    : constant Symbol := Intern ("pragma");
   Name_Private   : constant Symbol := Intern ("private");
   Name_Procedure : constant Symbol := Intern ("procedure");
   Name_Raise     : constant Symbol := Intern ("raise");
   Name_Range     : constant Symbol := Intern ("range");
   Name_Record    : constant Symbol := Intern ("record");
   Name_Rem       : constant Symbol := Intern ("rem");
   Name_Renames   : constant Symbol := Intern ("renames");
   Name_Return    : constant Symbol := Intern ("return");
   Name_Reverse   : constant Symbol := Intern ("reverse");
   Name_Select    : constant Symbol := Intern ("select");
   Name_Separate  : constant Symbol := Intern ("separate");
   Name_Subtype   : constant Symbol := Intern ("subtype");
   Name_Task      : constant Symbol := Intern ("task");
   Name_Terminate : constant Symbol := Intern ("terminate");
   Name_Then      : constant Symbol := Intern ("then");
   Name_Type      : constant Symbol := Intern ("type");
   Name_Use       : constant Symbol := Intern ("use");
   Name_When      : constant Symbol := Intern ("when");
   Name_While     : constant Symbol := Intern ("while");
   Name_With      : constant Symbol := Intern ("with");
   Name_Xor       : constant Symbol := Intern ("xor");

   --  Ada 95 reserved words

   Name_Abstract  : constant Symbol := Intern ("abstract");
   Name_Aliased   : constant Symbol := Intern ("aliased");
   Name_Protected : constant Symbol := Intern ("protected");
   Name_Until     : constant Symbol := Intern ("until");
   Name_Requeue   : constant Symbol := Intern ("requeue");
   Name_Tagged    : constant Symbol := Intern ("tagged");

   --  Ada 2005 reserved words

   Name_Interface    : constant Symbol := Intern ("interface");
   Name_Overriding   : constant Symbol := Intern ("overriding");
   Name_Synchronized : constant Symbol := Intern ("synchronized");

   --  Ada 2012 reserved words

   Name_Some : constant Symbol := Intern ("some");

   Last_Reserved_Word_Sym : constant Symbol := Last_Symbol;
   subtype Potential_Reserved_Word_Sym is Symbol range
     First_Reserved_Word_Sym .. Last_Reserved_Word_Sym;
   --  Might include some non-reserved-word symbols, depending on the order in
   --  which things get interned.

   --  Miscellaneous useful Symbols:

   Name_Empty : constant Symbol := Intern ("");
   Name_NL    : constant Symbol :=
     W_Intern ([1 => Utils.String_Utilities.NL]);
   Name_CR    : constant Symbol := W_Intern ([1 => Utils.String_Utilities.CR]);
   Name_CRLF  : constant Symbol := Intern (ASCII.CR & ASCII.LF);

   Name_Semicolon : constant Symbol := Intern (";");
   Name_L_Paren   : constant Symbol := Intern ("(");
   Name_R_Paren   : constant Symbol := Intern (")");
   Name_Colon     : constant Symbol := Intern (":");
   Name_Assign    : constant Symbol := Intern (":=");
   Name_Bang      : constant Symbol := Intern ("!");
   Name_Bar       : constant Symbol := Intern ("|");
   Name_Arrow     : constant Symbol := Intern ("=>");
   Name_Dot       : constant Symbol := Intern (".");
   Name_Tick      : constant Symbol := Intern ("'");

   Name_And_Then : constant Symbol := Intern ("and then");
   Name_Or_Else  : constant Symbol := Intern ("or else");

   Name_Q_And : constant Symbol := Intern ("""and""");
   Name_Q_Or  : constant Symbol := Intern ("""or""");
   Name_Q_Xor : constant Symbol := Intern ("""xor""");
   Name_Q_Mod : constant Symbol := Intern ("""mod""");
   Name_Q_Rem : constant Symbol := Intern ("""rem""");
   Name_Q_Abs : constant Symbol := Intern ("""abs""");
   Name_Q_Not : constant Symbol := Intern ("""not""");

   Name_Depends : constant Symbol := Intern ("Depends");
   Name_Refined_Depends : constant Symbol := Intern ("Refined_Depends");
   Name_Elab_Spec : constant Symbol := Intern ("Elab_Spec");
   Name_Elab_Body : constant Symbol := Intern ("Elab_Body");

   Name_Tab_Insertion_Point : constant Symbol :=
     Intern ("tab insertion point");
   Name_Tab_In_Out : constant Symbol := Intern ("tab in out");
   Name_Dot_Dot : constant Symbol := Intern ("..");
   Name_R_Sq : constant Symbol := Intern ("]");

   Name_Page : constant Symbol := Intern ("page");
   Name_Space : constant Symbol := Intern (" ");

end Utils.Predefined_Symbols;
