with System;

with Ada.Characters.Handling; use Ada.Characters.Handling;
pragma Warnings (Off, "internal GNAT unit");
with System.String_Hash;
pragma Warnings (On, "internal GNAT unit");

package body Utils.Generic_Symbols is

   --  We don't worry about reclaiming storage here.  Once a Symbol is
   --  created, it lasts forever, even if every reference to that Symbol
   --  has been deleted.  That's probably not a problem, since
   --  people recompile more-or-less the same code over and over,
   --  introducing new identifiers fairly rarely.

   --  Trying to garbage collect this data structure would be a real pain.
   --  And this data structure is so simple and efficient, that we'll
   --  tolerate minor storage leaks.

   Debug : constant Boolean := False;

   function Is_Proper_Symbol (S : Symbol) return Boolean is
   begin
      return S /= null;
   end Is_Proper_Symbol;

   --  Statistics:

   type Statistics_Rec is record
      Count      : Natural := 0; -- number of symbols created so far
      Char_Count : Natural := 0; -- total number of characters in them
      Byte_Count : Natural := 0; -- number of bytes allocated for them
      Word_Count : Natural := 0; -- number of words allocated for them
   end record;

   function Hash_String is new System.String_Hash.Hash
     (Character, String, Hash_Type);

   --  Note that there are two kinds of hashing going on here.
   --  The Hash_String function is used in the creation of Symbols
   --  themselves.  The Hash function is used by a child package, and
   --  assigns hash values simply by incrementing a global counter every
   --  time a Symbol is created.

   Lg_Hash_Table_Size : constant := 16;
   Hash_Table_Size    : constant := 2**Lg_Hash_Table_Size;

   Hash_Table : array (Hash_Type range 0 .. Hash_Table_Size - 1) of Symbol :=
     (others => null);
   --  Each component is the head of a null-terminated linear chain of
   --  Symbol_Recs, linked through Same_Hash_Link.
   --  New Symbol_Recs are linked in at the head.
   --  The hash table is fixed power-of-2 size.
   --  The experts tell us to use a prime number, but I'm not sure that's
   --  wise; using a power of 2 makes calculation of the hash value efficient.
   --  I don't know of an efficient and simple way to make it growable,
   --  so we'll just have to make it "plenty big".

   pragma Atomic_Components (Hash_Table);

   function Match_Same (Chain : Symbol; S : String) return Symbol;
   --  Chain points to the beginning of a hash chain (a component of
   --  Hash_Table).  This searches down the chain for an entry equal
   --  to S (null if not found).

   function Match_Lower (Chain : Symbol; Lower : String) return Symbol;
   --  Same as Match_Same, except it is case insensitive.
   --  Lower is expected to be lower case.

   function Match_Same (Chain : Symbol; S : String) return Symbol is
      Ptr : Symbol := Chain;
   begin
      while Is_Proper_Symbol (Ptr) and then Ptr.Chars.S /= S loop
         Ptr := Ptr.Same_Hash_Link;
      end loop;
      return Ptr;
   end Match_Same;

   function Match_Lower (Chain : Symbol; Lower : String) return Symbol is
      Ptr : Symbol := Chain;
   begin
      while Is_Proper_Symbol (Ptr) and then To_Lower (Ptr.Chars.S) /= Lower
      loop
         Ptr := Ptr.Same_Hash_Link;
      end loop;
      return Ptr;
   end Match_Lower;

   Next_Hash_Type : Hash_Type := Hash_Type'First;
   --  Each time a new Symbol is created, we store this in the
   --  Same_Ignoring_Case component, and increment it.

   protected Protector is

      procedure Protected_Intern
        (S, Lower :     String;
         H        :     Hash_Type;
         Chain    :     Symbol;
         Result   : out Symbol);
      --  This interns the string S in the table; called from Intern.
      --  Lower must be the lower-case equivalent of S.
      --  H is the hash value of Lower, and Chain is the value of
      --  Hash_Table(H).
      --  The result is returned in Result.
      --  Hash_Table(H) is updated if it was
      --  necessary to create a new Symbol_Rec.

      pragma Warnings (Off);
      function Get_Statistics return Statistics_Rec;
      pragma Warnings (On);

   private

      Stats : Statistics_Rec;

   end Protector;

   protected body Protector is

      --  Protected_Intern first searches the chain case sensitively
      --  (Match_Same), to find one equal to S.  If found, return it.
      --  This can only happen if the caller did *not* find it, but
      --  some other task sneaked in and created it before we entered
      --  Protected_Intern.
      --
      --  Otherwise (not found), we need to create a new one.
      --  Search again, case insensitively (Match_Lower).
      --  If found, set our Same_Ignoring_Case to that one's,
      --  otherwise to a new Hash_Type.
      --  Link it in at the start of the hash chain.

      procedure Protected_Intern
        (S, Lower :     String;
         H        :     Hash_Type;
         Chain    :     Symbol;
         Result   : out Symbol)
      is
      begin
         Result := Match_Same (Chain, S); -- Check *again*, this time locked.
         if Is_Proper_Symbol (Result) then
            return;
         end if;

         declare -- Here if we need to create a new Symbol_Rec.
            Temp               : constant Symbol := Match_Lower (Chain, Lower);
            Same_Ignoring_Case : Hash_Type;
         begin
            if Is_Proper_Symbol (Temp) then
               Same_Ignoring_Case := Temp.Same_Ignoring_Case;
            else
               Same_Ignoring_Case := Next_Hash_Type;
               Next_Hash_Type     := Next_Hash_Type + 1;
            end if;
            Result :=
              new Symbol_Rec'
                (Length             => S'Length,
                 Same_Hash_Link     => Chain,
                 Same_Ignoring_Case => Same_Ignoring_Case,
                 Reserved_Word      => No_Ada_Version,
                 Chars              => (Length => S'Length, S => S));
            Hash_Table (H) := Result; -- link it in (atomic write)

            --  Gather statistics:
            if True then -- ???
               Stats.Count      := Stats.Count + 1;
               Stats.Char_Count := Stats.Char_Count + S'Length;
               Stats.Byte_Count :=
                 Stats.Byte_Count + Result.all'Size / System.Storage_Unit;
               Stats.Word_Count :=
                 Stats.Word_Count + (Result.all'Size + 31) / 32;
            end if;
         end;
      end Protected_Intern;

      function Get_Statistics return Statistics_Rec is
      begin
         return Stats;
      end Get_Statistics;

   end Protector;

   function Hash_Symbol (S : Symbol) return Hash_Type is
   begin
      return S.Same_Ignoring_Case;
   end Hash_Symbol;

   function Intern (S : String) return Symbol is
      Lower  : constant String    := To_Lower (S); -- case folded
      H      : constant Hash_Type := Hash_String (Lower) mod Hash_Table'Length;
      Chain  : constant Symbol    := Hash_Table (H); -- atomic read
      Result : Symbol             := Match_Same (Chain, S);
   begin
      if not Is_Proper_Symbol (Result) then
         Protector.Protected_Intern (S, Lower, H, Chain, Result);
      end if;
      pragma Assert (Is_Proper_Symbol (Result));
      return Result;
   end Intern;

   --  Note that Intern first uses Match_Same to search for an exact
   --  match.  If found, we are done.  Otherwise, we call the
   --  Protected_Intern operation, which calls Match_Same *again*, this
   --  time under lock.  This is necessary to avoid a race condition.
   --  The first call to Match_Same (outside the lock) is not necessary;
   --  it is an optimization.  This works because Hash_Table has atomic
   --  components.
   --  Note that Protected_Intern takes a parameter H, indicating which
   --  component of Hash_Table is to be updated, as well as Chain, which
   --  is equal to Hash_Table(H).  One might imagine that
   --  it would be cleaner to pass Chain as an 'in out'
   --  parameter, instead.  However, that would cause a race condition;
   --  it is important that Hash_Table(H) be updated under lock
   --  (and note that the Symbol would be passed by copy in/out).

   --  Abort safety: The only variables declared outside the protected
   --  record are Hash_Table and Symbols_Pool.  These are only
   --  *updated* inside the protected record, which implies abort
   --  deferral.

   function Intern (Buf : Bounded_Str) return Symbol is
   begin
      return Intern (+Buf);
   end Intern;

   function Intern_Reserved_Word
     (S : String; Ada_Version : Ada_Version_Type) return Symbol is
   begin
      return Result : constant Symbol := Intern (S) do
         pragma Assert (Result.Reserved_Word = No_Ada_Version);
         Result.Reserved_Word := Ada_Version;
      end return;
   end Intern_Reserved_Word;

   function Is_Reserved_Word
     (S : Symbol; Ada_Version : Ada_Version_Type) return Boolean is
   begin
      return S.Reserved_Word <= Ada_Version;
   end Is_Reserved_Word;

   function Lookup (S : String; Fold_Case : Boolean) return Symbol is
      Lower : constant String    := To_Lower (S); -- case folded
      H     : constant Hash_Type := Hash_String (Lower) mod Hash_Table'Length;
      Chain : constant Symbol    := Hash_Table (H); -- atomic read
   begin
      if Fold_Case then
         return Match_Lower (Chain, Lower);
      else
         return Match_Same (Chain, S);
      end if;
   end Lookup;

   --  Efficiency properties:
   --  We presume that Equal is more common that Intern.
   --  Equal works in constant time, so that's good.
   --  The compiler should be able to eliminate the "if Fold_Case",
   --  because Equal is inlined, and Fold_Case is normally static
   --  at the call site.
   --  We also presume that in the steady state, the same strings will
   --  be Interned over and over.  Therefore, we optimize Intern to
   --  avoid entering the protected object in the usual case where the
   --  same string has already been interned.  Similarly, Lookup never
   --  modifies anything, and does not need to enter the protected
   --  object.
   --  Efficiency is not critical for the other operations.
   --  TBD: I would like somebody to review this trickery,
   --  with the Atomic_Components business, and the protected record,
   --  and verify that there really are no race conditions.
   --  And abort safety.

   --  TBD: Actually, we could make this abort safe without the
   --  protected record, as follows:
   --  Change Subpools to be semi-abort-safe, in the sense that an abort
   --  can leak one allocation, but not damage data structures.
   --  Make Symbols_Pool be such a Subpool.
   --  Eliminate the protected record, but keep the pragma
   --  Atomic_Components.
   --  Then an abort during Intern could leak one Symbol_Rec;
   --  the next time the same string is Interned would create another
   --  one.  That doesn't seem like a big deal.  On the other hand,
   --  if it really is rare (in the steady state) to call
   --  Protected_Intern, then we don't gain much efficiency by
   --  eliminating it, and we lose task safety.

   --  We could avoid copying the string in Intern, but that would make our
   --  storage management and abort-deferral problems even more onerous.
   --  Better to let the client (probably some lexer) allocate its own local
   --  buffer for the string, and have Intern copy it (if it's never been
   --  seen before).

   function Str (S : Symbol) return Access_Constant_String_Rec is
   begin
      return S.Chars'Access;
   end Str;

   function Case_Sensitive_Equal (S1, S2 : Symbol) return Boolean is
      pragma Assert (Is_Proper_Symbol (S1) and Is_Proper_Symbol (S2));
      Result : constant Boolean := S1 = S2;
   begin
      if Debug then
         pragma Assert (Result = (Str (S1).S = Str (S2).S));
         null;
      end if;
      return Result;
   end Case_Sensitive_Equal;

   function Case_Insensitive_Equal (S1, S2 : Symbol) return Boolean is
      pragma Assert (Is_Proper_Symbol (S1) and Is_Proper_Symbol (S2));
      Result : constant Boolean :=
        S1.Same_Ignoring_Case = S2.Same_Ignoring_Case;
   begin
      if Debug then
         pragma Assert
           (Result = (To_Lower (Str (S1).S) = To_Lower (Str (S2).S)));
         null;
      end if;
      return Result;
   end Case_Insensitive_Equal;

   function Symbols_Equal
     (S1, S2    : Symbol;
      Fold_Case : Boolean) return Boolean
   is
   begin
      if Fold_Case then
         return Case_Insensitive_Equal (S1, S2);
      else
         return Case_Sensitive_Equal (S1, S2);
      end if;
   end Symbols_Equal;

   function "<" (S1, S2 : Symbol) return Boolean is
   begin
      return Str (S1).S < Str (S2).S;
   end "<";

   function "<=" (S1, S2 : Symbol) return Boolean is
   begin
      return Str (S1).S <= Str (S2).S;
   end "<=";

   function ">" (S1, S2 : Symbol) return Boolean is
   begin
      return Str (S1).S > Str (S2).S;
   end ">";

   function ">=" (S1, S2 : Symbol) return Boolean is
   begin
      return Str (S1).S >= Str (S2).S;
   end ">=";

   function "&" (S1 : Symbol; S2 : String) return Symbol is
   begin
      return Intern (Str (S1).S & S2);
   end "&";

   function "&" (S1 : String; S2 : Symbol) return Symbol is
   begin
      return Intern (S1 & Str (S2).S);
   end "&";

--    procedure Print_Stats_Method is
--        Stats: constant Statistics_Rec := Protector.Get_Statistics;
--    begin
--        Put_Line("Symbols statistics:");
--        Put_Line("  Symbols_Count = " & Image(Stats.Count));
--        Put_Line("  Symbols_Char_Count = " & Image(Stats.Char_Count));
--        Put_Line("  Symbols_Byte_Count = " & Image(Stats.Byte_Count));
--        Put_Line("  Symbols_Word_Count = " & Image(Stats.Word_Count));
--    end Print_Stats_Method;

   function Last_Symbol return Symbol_Index'Base is
   begin
      return Symbol_Index (Next_Hash_Type);
   end Last_Symbol;

   function Get_Symbol_Index (S : Symbol) return Symbol_Index is
   begin
      return Symbol_Index (S.Same_Ignoring_Case + 1);
   end Get_Symbol_Index;

end Utils.Generic_Symbols;
