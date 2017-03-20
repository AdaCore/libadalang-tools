-- CC1010A.ADA

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
-- CHECK THAT THE NAMES IN A GENERIC SUBPROGRAM DECLARATION ARE
-- STATICALLY IDENTIFIED (I.E., BOUND) AT THE POINT WHERE THE
-- GENERIC DECLARATION TEXTUALLY OCCURS, AND ARE NOT DYNAMICALLY
-- BOUND AT THE POINT OF INSTANTIATION.

-- ASL 8/12/81

with Report;
procedure Cc1010a is
   use Report;
begin
   Test
     ("CC1010A",
      "PROPER VISIBILITY OF FREE IDENTIFIERS IN " &
      "GENERIC DECLARATIONS, BODIES AND INSTANTIATIONS");

   Outer : declare
      Free : constant Integer := 5;
   begin
      declare
         generic
            Gfp : Integer := Free;
         procedure P (Pfp : in Integer := Free);

         Free : constant Integer := 6;

         procedure P (Pfp : in Integer := Outer.Free) is
         begin
            if Free /= 6 or Gfp /= 5 or Pfp /= 5 then
               Failed ("BINDINGS INCORRECT");
            end if;
         end P;
      begin
         declare
            Free : constant Integer := 7;
            procedure Inst is new P;
         begin
            Inst;
         end;
      end;
   end Outer;
   Result;
end Cc1010a;
