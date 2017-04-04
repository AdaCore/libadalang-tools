-- C43104A.ADA

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
--     CHECK THAT WITH THE TYPE OF THE AGGREGATE RESOLVED, THE
--     DISCRIMINANT MAY BE USED TO DECIDE TO WHICH OF THE VARIANT'S
--     SUBTYPES THE AGGREGATE BELONGS.

-- HISTORY:
--     DHH 08/08/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43104a is

   type Int is range 0 .. 10;

   type Var_Rec (Bool : Boolean := True) is record
      case Bool is
         when True =>
            X : Integer;
         when False =>
            Y : Int;
      end case;
   end record;

   subtype S_True is Var_Rec (True);
   subtype S_False is Var_Rec (False);

   procedure Check (P : in S_True) is
   begin
      if P.Bool = False then
         Failed ("WRONG PROCEDURE ENTERED");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED INSIDE PROCEDURE");

   end Check;

begin
   Test
     ("C43104A",
      "CHECK THAT WITH THE TYPE OF THE AGGREGATE " &
      "RESOLVED, THE DISCRIMINANT MAY BE USED TO " &
      "DECIDE TO WHICH OF THE VARIANT'S SUBTYPES " &
      "THE AGGREGATE BELONGS");

   Check ((True, 1));

   begin

      Check ((False, 2));
      Failed
        ("PROCEDURE CALL USING '(FALSE, 2)' DID NOT RAISE " & "EXCEPTION");

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("INCORRECT EXCEPTION RAISED ON PROCEDURE CALL " &
            "USING '(FALSE,2)'");
   end;

   Result;
end C43104a;
