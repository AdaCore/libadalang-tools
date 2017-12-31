-- C46042A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK ARRAY CONVERSIONS WHEN THE TARGET TYPE IS A CONSTRAINED ARRAY TYPE
-- AND THE OPERAND TYPE HAS BOUNDS THAT DO NOT BELONG TO THE BASE TYPE OF THE
-- TARGET TYPE'S INDEX SUBTYPE.

-- R.WILLIAMS 9/8/86

with Report; use Report;
procedure C46042a is

   type Int is range -100 .. 100;

   type Newinteger is new Integer;

   type Day is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

   type Nday1 is new Day range Mon .. Fri;
   type Nday2 is new Day range Mon .. Fri;

   type Nnday1 is new Nday1;

   function Ident (X : Int) return Int is
   begin
      return Int'Val (Ident_Int (Int'Pos (X)));
   end Ident;

   function Ident (X : Newinteger) return Newinteger is
   begin
      return Newinteger'Val (Ident_Int (Newinteger'Pos (X)));
   end Ident;

   function Ident (X : Nday1) return Nday1 is
   begin
      return Nday1'Val (Ident_Int (Nday1'Pos (X)));
   end Ident;

   function Ident (X : Nday2) return Nday2 is
   begin
      return Nday2'Val (Ident_Int (Nday2'Pos (X)));
   end Ident;

   function Ident (X : Nnday1) return Nnday1 is
   begin
      return Nnday1'Val (Ident_Int (Nnday1'Pos (X)));
   end Ident;

begin
   Test
     ("C46042A",
      "CHECK ARRAY CONVERSIONS WHEN THE TARGET " &
      "TYPE IS A CONSTRAINED ARRAY TYPE AND THE " &
      "OPERAND TYPE HAS BOUNDS THAT DO NOT " &
      "BELONG TO THE BASE TYPE OF THE TARGET " & "TYPE'S INDEX SUBTYPE");

   declare

      type Unarr1 is array (Integer range <>) of Integer;
      subtype Conarr1 is Unarr1 (Ident_Int (1) .. Ident_Int (10));

      type Unarr2 is array (Integer range <>, Nday1 range <>) of Integer;
      subtype Conarr2 is
        Unarr2 (Ident_Int (1) .. Ident_Int (10), Ident (Mon) .. Ident (Tue));

      type Arr1 is array (Int range <>) of Integer;
      A1 : Arr1 (Ident (11) .. Ident (20)) := (Ident (11) .. Ident (20) => 0);

      type Arr2 is array (Int range <>, Nday2 range <>) of Integer;
      A2 : Arr2 (Ident (11) .. Ident (20), Ident (Wed) .. Ident (Thu)) :=
        (Ident (11) .. Ident (20) => (Ident (Wed) .. Ident (Thu) => 0));

      type Arr3 is array (Newinteger range <>, Nnday1 range <>) of Integer;
      A3 : Arr3 (Ident (11) .. Ident (20), Ident (Wed) .. Ident (Thu)) :=
        (Ident (11) .. Ident (20) => (Ident (Wed) .. Ident (Thu) => 0));

      procedure Check (A : Unarr1) is
      begin
         if A'First /= 1 or A'Last /= 10 then
            Failed ("INCORRECT CONVERSION OF UNARR1 (A1)");
         end if;
      end Check;

      procedure Check (A : Unarr2; Str : String) is
      begin
         if A'First (1) /= 1 or A'Last /= 10 or A'First (2) /= Mon or
           A'Last (2) /= Tue then
            Failed ("INCORRECT CONVERSION OF UNARR2 (A" & Str & ")");
         end if;
      end Check;

   begin
      begin
         Check (Conarr1 (A1));
      exception
         when others =>
            Failed ("EXCEPTION RAISED BY 'CONARR1 (A1)'");
      end;

      begin
         Check (Conarr2 (A2), "2");
      exception
         when others =>
            Failed ("EXCEPTION RAISED BY 'CONARR2 (A2)'");
      end;

      begin
         Check (Conarr2 (A3), "3");
      exception
         when others =>
            Failed ("EXCEPTION RAISED BY 'CONARR2 (A3)'");
      end;

   end;

   Result;
end C46042a;
