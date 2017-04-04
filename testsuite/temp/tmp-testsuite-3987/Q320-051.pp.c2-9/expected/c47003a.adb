-- C47003A.ADA

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
-- WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES AN
-- ENUMERATION TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE
-- VALUE OF THE OPERAND DOES NOT LIE WITHIN THE RANGE OF THE TYPE MARK.

-- RJW 7/23/86

with Report; use Report;
procedure C47003a is

begin

   Test
     ("C47003A",
      "WHEN THE TYPE MARK IN A QUALIFIED " &
      "EXPRESSION DENOTES AN ENUMERATION " &
      "TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED " &
      "WHEN THE VALUE OF THE OPERAND DOES NOT LIE " &
      "WITHIN THE RANGE OF THE TYPE MARK");

   declare

      type Week is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      subtype Midweek is Week range Tue .. Thu;

      function Ident (W : Week) return Week is
      begin
         return Week'Val (Ident_Int (Week'Pos (W)));
      end Ident;

   begin
      if Midweek'(Ident (Sun)) = Tue then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
            "SUBTYPE MIDWEEK - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
            "SUBTYPE MIDWEEK - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " &
            "OF SUBTYPE MIDWEEK");
   end;

   declare

      subtype Char is Character range 'C' .. 'R';

   begin
      if Char'(Ident_Char ('A')) = 'C' then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE CHAR - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE CHAR - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " & "OF SUBTYPE CHAR");
   end;

   declare

      type Nbool is new Boolean;
      subtype Nfalse is Nbool range False .. False;

      function Ident (B : Nbool) return Nbool is
      begin
         return Nbool (Ident_Bool (Boolean (B)));
      end Ident;

   begin
      if Nfalse'(Ident (True)) = False then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
            "SUBTYPE NFALSE - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
            "SUBTYPE NFALSE - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " & "OF SUBTYPE NFALSE");
   end;

   Result;
end C47003a;
