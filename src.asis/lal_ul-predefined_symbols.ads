with LAL_UL.Symbols; use LAL_UL.Symbols;

package LAL_UL.Predefined_Symbols is

   function R83
     (S : String; Ada_Version : Ada_Version_Type := Ada_83) return Symbol
     renames Intern_Reserved_Word;

   function R95
     (S : String; Ada_Version : Ada_Version_Type := Ada_95) return Symbol
     renames Intern_Reserved_Word;

   function R2005
     (S : String; Ada_Version : Ada_Version_Type := Ada_2005) return Symbol
     renames Intern_Reserved_Word;

   function R2012
     (S : String; Ada_Version : Ada_Version_Type := Ada_2012) return Symbol
     renames Intern_Reserved_Word;

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

   --  Other names

   Name_Page : constant Symbol := Intern ("page");

end LAL_UL.Predefined_Symbols;
