-- C47006A.ADA

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
-- WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES A FIXED POINT
-- TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE VALUE OF THE
-- OPERAND DOES NOT LIE WITHIN THE RANGE OF THE TYPE MARK.

-- RJW 7/23/86

with Report; use Report;
procedure C47006a is

   type Fixed is delta 0.5 range -5.0 .. 5.0;

begin

   Test
     ("C47006A",
      "WHEN THE TYPE MARK IN A QUALIFIED " &
      "EXPRESSION DENOTES A FIXED POINT TYPE, " &
      "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
      "WHEN THE VALUE OF THE OPERAND DOES NOT LIE " &
      "WITHIN THE RANGE OF THE TYPE MARK");

   declare

      subtype Sfixed is Fixed range -2.0 .. 2.0;

      function Ident (X : Fixed) return Fixed is
      begin
         if Equal (3, 3) then
            return X;
         else
            return 0.0;
         end if;
      end Ident;

   begin
      if Sfixed'(Ident (-5.0)) = -2.0 then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
            "SUBTYPE SFIXED - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
            "SUBTYPE SFIXED - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " & "OF SUBTYPE SFIXED");
   end;

   declare

      type Nfix is new Fixed;
      subtype Snfix is Nfix range -2.0 .. 2.0;

      function Ident (X : Nfix) return Nfix is
      begin
         return Nfix (Ident_Int (Integer (X)));
      end Ident;

   begin
      if Snfix'(Ident (-5.0)) = -2.0 then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE SNFIX - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE SNFIX - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " & "OF SUBTYPE SNFIX");
   end;

   Result;
end C47006a;
