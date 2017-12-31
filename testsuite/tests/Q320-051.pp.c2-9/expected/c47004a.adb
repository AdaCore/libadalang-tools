-- C47004A.ADA

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
-- WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES AN INTEGER TYPE, CHECK
-- THAT CONSTRAINT_ERROR IS RAISED WHEN THE VALUE OF THE OPERAND DOES NOT LIE
-- WITHIN THE RANGE OF THE TYPE MARK.

-- RJW 7/23/86

with Report; use Report;
procedure C47004a is

begin

   Test
     ("C47004A",
      "WHEN THE TYPE MARK IN A QUALIFIED " & "EXPRESSION DENOTES AN INTEGER " &
      "TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED " &
      "WHEN THE VALUE OF THE OPERAND DOES NOT LIE " &
      "WITHIN THE RANGE OF THE TYPE MARK");

   declare

      type Int is range -10 .. 10;
      subtype Sint is Int range -5 .. 5;

      function Ident (I : Int) return Int is
      begin
         return Int (Ident_Int (Integer (I)));
      end Ident;

   begin
      if Sint'(Ident (10)) = 5 then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE SINT - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE SINT - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " & "OF SUBTYPE SINT");
   end;

   declare

      subtype Sinteger is Integer range -10 .. 10;

   begin
      if Sinteger'(Ident_Int (20)) = 15 then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
            "SUBTYPE SINTEGER - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
            "SUBTYPE SINTEGER - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " &
            "OF SUBTYPE SINTEGER");
   end;

   declare

      type Ninteger is new Integer;
      subtype Snint is Ninteger range -10 .. 10;

      function Ident (I : Ninteger) return Ninteger is
      begin
         return Ninteger (Ident_Int (Integer (I)));
      end Ident;

   begin
      if Snint'(Ident (-20)) = -10 then
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE SNINT - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " & "SUBTYPE SNINT - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " & "OF SUBTYPE SNINT");
   end;

   Result;
end C47004a;
