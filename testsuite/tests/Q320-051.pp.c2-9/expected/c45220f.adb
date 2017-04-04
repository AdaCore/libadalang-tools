-- C45220F.ADA

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
-- CHECK THAT THE MEMBERSHIP OPERATIONS WORK CORRECTLY FOR DERIVED BOOLEAN
-- TYPES.

-- GLH 08/01/85

with Report;
procedure C45220f is

   use Report;

begin

   Test ("C45220F", "CHECK MEMBERSHIP OPERATIONS FOR " & "DERIVED BOOLEAN");

   declare

      type Newbool is new Boolean;

      Var : Newbool          := False;
      Con : constant Newbool := False;

   begin

      if True not in Newbool or Var not in Newbool or Con not in Newbool then
         Failed ("WRONG VALUES FOR 'IN NEWBOOL'");
      end if;

      if Newbool'(False) in True .. False or
        Var not in False .. True or
        Con in True .. True
      then
         Failed ("WRONG VALUES FOR 'IN AAA..BBB'");
      end if;

      Result;

   end;

end C45220f;
