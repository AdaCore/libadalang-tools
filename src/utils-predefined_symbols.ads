with Utils.Symbols; use Utils.Symbols;
with Utils.String_Utilities;

package Utils.Predefined_Symbols is

   function R83 (S : String) return Symbol is
     (Intern_Reserved_Word (S, Ada_Version => Ada_83));

   function R95 (S : String) return Symbol is
     (Intern_Reserved_Word (S, Ada_Version => Ada_95));

   function R2005 (S : String) return Symbol is
     (Intern_Reserved_Word (S, Ada_Version => Ada_2005));

   function R2012 (S : String) return Symbol is
     (Intern_Reserved_Word (S, Ada_Version => Ada_2012));

   --  Ada 83 reserved words

   Name_Abort     : constant Symbol := R83 ("abort");
   Name_Abs       : constant Symbol := R83 ("abs");
   Name_Accept    : constant Symbol := R83 ("accept");
   Name_Access    : constant Symbol := R83 ("access");
   Name_And       : constant Symbol := R83 ("and");
   Name_All       : constant Symbol := R83 ("all");
   Name_Array     : constant Symbol := R83 ("array");
   Name_At        : constant Symbol := R83 ("at");
   Name_Begin     : constant Symbol := R83 ("begin");
   Name_Body      : constant Symbol := R83 ("body");
   Name_Case      : constant Symbol := R83 ("case");
   Name_Constant  : constant Symbol := R83 ("constant");
   Name_Declare   : constant Symbol := R83 ("declare");
   Name_Delay     : constant Symbol := R83 ("delay");
   Name_Delta     : constant Symbol := R83 ("delta");
   Name_Digits    : constant Symbol := R83 ("digits");
   Name_Do        : constant Symbol := R83 ("do");
   Name_Else      : constant Symbol := R83 ("else");
   Name_Elsif     : constant Symbol := R83 ("elsif");
   Name_End       : constant Symbol := R83 ("end");
   Name_Entry     : constant Symbol := R83 ("entry");
   Name_Exception : constant Symbol := R83 ("exception");
   Name_Exit      : constant Symbol := R83 ("exit");
   Name_For       : constant Symbol := R83 ("for");
   Name_Function  : constant Symbol := R83 ("function");
   Name_Generic   : constant Symbol := R83 ("generic");
   Name_Goto      : constant Symbol := R83 ("goto");
   Name_If        : constant Symbol := R83 ("if");
   Name_In        : constant Symbol := R83 ("in");
   Name_Is        : constant Symbol := R83 ("is");
   Name_Limited   : constant Symbol := R83 ("limited");
   Name_Loop      : constant Symbol := R83 ("loop");
   Name_Mod       : constant Symbol := R83 ("mod");
   Name_New       : constant Symbol := R83 ("new");
   Name_Not       : constant Symbol := R83 ("not");
   Name_Null      : constant Symbol := R83 ("null");
   Name_Of        : constant Symbol := R83 ("of");
   Name_Or        : constant Symbol := R83 ("or");
   Name_Others    : constant Symbol := R83 ("others");
   Name_Out       : constant Symbol := R83 ("out");
   Name_Package   : constant Symbol := R83 ("package");
   Name_Pragma    : constant Symbol := R83 ("pragma");
   Name_Private   : constant Symbol := R83 ("private");
   Name_Procedure : constant Symbol := R83 ("procedure");
   Name_Raise     : constant Symbol := R83 ("raise");
   Name_Range     : constant Symbol := R83 ("range");
   Name_Record    : constant Symbol := R83 ("record");
   Name_Rem       : constant Symbol := R83 ("rem");
   Name_Renames   : constant Symbol := R83 ("renames");
   Name_Return    : constant Symbol := R83 ("return");
   Name_Reverse   : constant Symbol := R83 ("reverse");
   Name_Select    : constant Symbol := R83 ("select");
   Name_Separate  : constant Symbol := R83 ("separate");
   Name_Subtype   : constant Symbol := R83 ("subtype");
   Name_Task      : constant Symbol := R83 ("task");
   Name_Terminate : constant Symbol := R83 ("terminate");
   Name_Then      : constant Symbol := R83 ("then");
   Name_Type      : constant Symbol := R83 ("type");
   Name_Use       : constant Symbol := R83 ("use");
   Name_When      : constant Symbol := R83 ("when");
   Name_While     : constant Symbol := R83 ("while");
   Name_With      : constant Symbol := R83 ("with");
   Name_Xor       : constant Symbol := R83 ("xor");

   --  Ada 95 reserved words

   Name_Abstract  : constant Symbol := R95 ("abstract");
   Name_Aliased   : constant Symbol := R95 ("aliased");
   Name_Protected : constant Symbol := R95 ("protected");
   Name_Until     : constant Symbol := R95 ("until");
   Name_Requeue   : constant Symbol := R95 ("requeue");
   Name_Tagged    : constant Symbol := R95 ("tagged");

   --  Ada 2005 reserved words

   Name_Interface    : constant Symbol := R2005 ("interface");
   Name_Overriding   : constant Symbol := R2005 ("overriding");
   Name_Synchronized : constant Symbol :=
     R2005 ("synchronized");

   --  Ada 2012 reserved words

   Name_Some : constant Symbol := R2012 ("some");

   --  Miscellaneous useful Symbols:

   Name_Empty : constant Symbol := Intern ("");
   Name_NL    : constant Symbol :=
     W_Intern ((1 => Utils.String_Utilities.NL));

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

end Utils.Predefined_Symbols;
