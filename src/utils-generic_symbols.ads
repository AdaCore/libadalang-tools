------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Containers; use Ada.Containers;
with Utils.String_Utilities; use Utils.String_Utilities;
generic
   --  Each instance has a separate table of strings
package Utils.Generic_Symbols is

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

   type Symbol is range 1 .. 2**24 - 1; -- represents a character string
   subtype Opt_Symbol is Symbol'Base range 0 .. Symbol'Last;
   No_Symbol : constant Opt_Symbol := Opt_Symbol'First;

   --  Symbol is publically an integer type so that we can create arrays and
   --  vectors indexed by it. Other arithmetic operations on Symbols don't make
   --  much sense.

   function No (S : Opt_Symbol) return Boolean is (S = No_Symbol);
   function Present (S : Opt_Symbol) return Boolean is (not No (S));
   pragma Inline (No, Present);

   function Intern (S : String) return Symbol;
   --  Convert S to a Symbol, adding it to the table if necessary.

   function Intern (Buf : Bounded_Str) return Symbol;

   function Lookup (S : String; Fold_Case : Boolean) return Opt_Symbol;
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

   function Last_Symbol return Opt_Symbol;

   function Same_Ignoring_Case (S : Symbol) return Symbol;
   --  Returns the first-created symbol that is the same as S (ignoring case).
   --  If there are no other such symbols, return S.

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

   function "&" (S1 : Symbol; S2 : String) return Symbol;
   function "&" (S1 : String; S2 : Symbol) return Symbol;

   function Hash_Symbol (S : Symbol) return Hash_Type;
   --  The hash is case insensitive; that is, for all Symbols S1 and S2,
   --  Symbols_Equal(S1, S2, Fold_Case => True) implies Hash(S1) = Hash(S2).
   --  This can be used to make a hash table mapping Symbols to whatever.

   procedure Print_Statistics;

   --  Wide_Strings:

   function W_Intern (S : Wide_String) return Symbol is (Intern (To_UTF8 (S)));
   function To_W_Str (S : Symbol) return Wide_String is
     (From_UTF8 (Str (S).S));

end Utils.Generic_Symbols;
