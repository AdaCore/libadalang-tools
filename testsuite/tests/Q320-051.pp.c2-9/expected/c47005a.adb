-- C47005A.ADA

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
-- OBJECTIVE:
--     WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES A FLOATING
--     POINT TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE VALUE
--     OF THE OPERAND DOES NOT LIE WITHIN THE RANGE OF THE TYPE MARK.

-- HISTORY:
--     RJW 07/23/86  CREATED ORIGINAL TEST.
--     BCB 08/19/87  CHANGED HEADER TO STANDARD HEADER FORMAT.  ADDED
--                   TEST FOR UPPER SIDE OF RANGE.

with Report; use Report;
procedure C47005a is

begin

   Test
     ("C47005A",
      "WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION " &
      "DENOTES A FLOATING POINT TYPE, CHECK THAT " &
      "CONSTRAINT_ERROR IS RAISED WHEN THE VALUE " &
      "OF THE OPERAND DOES NOT LIE WITHIN THE " & "RANGE OF THE TYPE MARK");

   declare

      subtype Sfloat is Float range -1.0 .. 1.0;

      function Ident (F : Float) return Float is
      begin
         if Equal (3, 3) then
            return F;
         else
            return 0.0;
         end if;
      end Ident;

   begin
      if Sfloat'(Ident (-2.0)) = -1.0 then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
            "SUBTYPE SFLOAT - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
            "SUBTYPE SFLOAT - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " & "OF SUBTYPE SFLOAT");
   end;

   declare

      type Flt is digits 3 range -5.0 .. 5.0;
      subtype Sflt is Flt range -1.0 .. 1.0;

      function Ident (F : Flt) return Flt is
      begin
         if Equal (3, 3) then
            return F;
         else
            return 0.0;
         end if;
      end Ident;

   begin
      if Sflt'(Ident (-2.0)) = -1.0 then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE SFLT - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE SFLT - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " & "OF SUBTYPE SFLT");
   end;

   declare

      type Nflt is new Float;
      subtype Snflt is Nflt range -1.0 .. 1.0;

      function Ident (F : Nflt) return Nflt is
      begin
         if Equal (3, 3) then
            return F;
         else
            return 0.0;
         end if;
      end Ident;

   begin
      if Snflt'(Ident (2.0)) = 1.0 then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE SNFLT 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE SNFLT 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " & "OF SUBTYPE SNFLT");
   end;

   Result;
end C47005a;
