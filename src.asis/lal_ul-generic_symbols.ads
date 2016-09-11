with Ada.Containers; use Ada.Containers;
with ASIS_UL.String_Utilities; use ASIS_UL.String_Utilities;
generic
   --  Each instance has a separate table of strings
package LAL_UL.Generic_Symbols is

   --  Representation of Strings -- like Lisp "symbols".

   --  Adapted from CodePeer's Spellings package.

   --  This is intended for representing things like identifiers in a
   --  programming language.  The characters of each unique symbol are
   --  stored only once, thus reducing memory use, and making equality
   --  comparisons efficient.  Typically, the lexical analysis phase
   --  creates Symbol values from Strings by calling Intern.  Later phases
   --  use Equal for efficient comparison and/or use Symbols.Tables for
   --  hashed lookups.

   --  Case is preserved, and both case sensitive and insensitive efficient
   --  equality comparisons are provided.

   --  This package is abort safe and task safe.
   --  If the same string is Interned by two tasks at the
   --  same time, they will both return the *same* Symbol.
   --  An abort in the middle of Intern will not damage data structures
   --  or leak memory.

   type Symbol is private; -- represents a character string
   No_Symbol : constant Symbol;

   function Is_Proper_Symbol (S : Symbol) return Boolean;
   --  True if it's not No_Symbol.
   pragma Inline (Is_Proper_Symbol);

   function Intern (S : String) return Symbol;
   --  Convert S to a Symbol, adding it to the table if necessary.

   function Intern (Buf : Bounded_Str) return Symbol;

   function Intern_Reserved_Word
     (S : String; Ada_Version : Ada_Version_Type) return Symbol;
   --  Same as Intern, but indicates that this is an Ada reserved word. We
   --  could generalize this and make it more extensible, but for now, all we
   --  need is this one flag. Ada_Version is the first version of Ada in which
   --  the word was reserved.
   function Is_Reserved_Word
     (S : Symbol; Ada_Version : Ada_Version_Type) return Boolean;
   --  True if S is a reserved word in the specified version of Ada.

   function Lookup (S : String; Fold_Case : Boolean) return Symbol;
   --  If a symbol equal to S (given Fold_Case) already exists,
   --  return it.  Otherwise, returns No_Symbol.
   --  Note: If Fold_Case is True, there could be several matches;
   --  we return an arbitrary one of them.
   --  Note: the table of symbols is never changed by this,
   --  so it can be used instead of Intern in cases where we want to avoid
   --  eating up memory.

   type String_Rec (Length : Natural) is -- String with 'First constrained to 1
   record
      S : String (1 .. Length);
   end record;

   type Access_Constant_String_Rec is access constant String_Rec;
   for Access_Constant_String_Rec'Storage_Size use 0;
   function Str (S : Symbol) return Access_Constant_String_Rec;
   pragma Inline (Str);
   --  To convert a Symbol X back into a String, say "Str(X).S"

   function Case_Sensitive_Equal (S1, S2 : Symbol) return Boolean;
   function Case_Insensitive_Equal (S1, S2 : Symbol) return Boolean;

   function Symbols_Equal
     (S1, S2    : Symbol;
      Fold_Case : Boolean) return Boolean;

   --  Rationale: Even though some languages are case insensitive, we want
   --  to preserve case for printing civilized error messages.
   --  Even though some languages are case sensitive, we want to provide
   --  case insensitive comparisons, to make friendly error messages
   --  possible ("Perhaps when you said 'Mumble' you meant 'MUMBLE'.").
   --  And in any case, it's simplest to have just one version of this
   --  package.

   --  The following comparisons are provided for convenience, although
   --  they aren't as efficient as Equal.  These are case sensitive.
   function "<" (S1, S2 : Symbol) return Boolean;
   function "<=" (S1, S2 : Symbol) return Boolean;
   function ">" (S1, S2 : Symbol) return Boolean;
   function ">=" (S1, S2 : Symbol) return Boolean;

   function "&" (S1 : Symbol; S2 : String) return Symbol;
   function "&" (S1 : String; S2 : Symbol) return Symbol;

   function Hash_Symbol (S : Symbol) return Hash_Type;
   --  The hash is case insensitive; that is, for all Symbols S1 and S2,
   --  Symbols_Equal(S1, S2, Fold_Case => True) implies Hash(S1) = Hash(S2).
   --  This can be used to make a hash table mapping Symbols to whatever.

   type Symbol_Index is new Positive;
   subtype Symbol_Count is Symbol_Index'Base range 0 .. Symbol_Index'Last;
   function Last_Symbol return Symbol_Index'Base;
   --  Returns the highest value of Same_Ignoring_Case. It's important
   --  to use a signed type here, because we don't want modular arithmetic.
   --  Empty arrays should be 1 .. 0, not 0 .. 2**32 -1!

   function Get_Symbol_Index (S : Symbol) return Symbol_Index;
   --  Return a unique integer for the Symbol. This can be used to
   --  make arrays indexed by Symbol.

   --  Wide_Strings:

   function W_Intern (S : Wide_String) return Symbol is (Intern (To_UTF8 (S)));
   function To_W_Str (S : Symbol) return Wide_String is
     (From_UTF8 (Str (S).S));

private

   type Symbol_Rec (Length : Natural) is record
      Same_Hash_Link     : Symbol;
      Same_Ignoring_Case : Hash_Type;
      --  All Symbols that are the same ignoring case have
      --  Same_Ignoring_Case equal to some symbol in that group;
      --  they all point to the same one, so we can compare
      --  these to do case-insensitive equality comparisons.
      --  One of them (presumably the first one created) will point to
      --  itself.
      Reserved_Word : Opt_Ada_Version_Type;
      --  The earliest version of Ada in which this is a reserved word.
      --  If it's not a reserved word in the latest version, this is
      --  No_Ada_Version.
      Chars : aliased String_Rec (Length);
   end record;

   type Symbol is access Symbol_Rec;
   No_Symbol : constant Symbol := null;

end LAL_UL.Generic_Symbols;
