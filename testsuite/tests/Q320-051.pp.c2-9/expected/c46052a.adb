-- C46052A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR CONVERSION TO AN ENUMERATION TYPE
-- IF THE VALUE OF THE OPERAND DOES NOT BELONG TO THE RANGE OF ENUMERATION
-- VALUES FOR THE TARGET SUBTYPE.

-- R.WILLIAMS 9/9/86

with Report; use Report;
procedure C46052a is

   type Enum is (A, Ab, Abc, Abcd);
   E : Enum := Enum'Val (Ident_Int (0));

   function Ident (E : Enum) return Enum is
   begin
      return Enum'Val (Ident_Int (Enum'Pos (E)));
   end Ident;

begin
   Test
     ("C46052A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
      "CONVERSION TO AN ENUMERATION TYPE IF THE " &
      "VALUE OF THE OPERAND DOES NOT BELONG TO " &
      "THE RANGE OF ENUMERATION VALUES FOR THE " & "TARGET SUBTYPE");

   declare
      subtype Senum is Enum range Ab .. Abcd;
   begin
      E := Ident (Senum (E));
      Failed ("NO EXCEPTION RAISED FOR 'SENUM (E)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'SENUM (E)'");
   end;

   declare
      subtype Noenum is Enum range Abcd .. Ab;
   begin
      E := Ident (Noenum (E));
      Failed ("NO EXCEPTION RAISED FOR 'NOENUM (E)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'NOENUM (E)'");
   end;

   declare
      subtype Schar is Character range 'C' .. 'R';
      A : Character := Ident_Char ('A');
   begin
      A := Ident_Char (Schar (A));
      Failed ("NO EXCEPTION RAISED FOR 'SCHAR (A)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'SCHAR (A)'");
   end;

   declare
      subtype Frange is Boolean range False .. False;
      T : Boolean := Ident_Bool (True);
   begin
      T := Ident_Bool (Frange (T));
      Failed ("NO EXCEPTION RAISED FOR 'FRANGE (T)'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR 'FRANGE (T)'");
   end;

   Result;
end C46052a;
