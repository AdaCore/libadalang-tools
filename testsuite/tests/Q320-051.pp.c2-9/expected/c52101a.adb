-- C52101A.ADA

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
-- CHECK THAT ARRAY SUBTYPE CONVERSION IS APPLIED AFTER AN ARRAY VALUE
-- IS DETERMINED.

-- BHS 6/22/84

with Report;
procedure C52101a is

   use Report;

   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   subtype Weekday is Day range Mon .. Fri;

   type Arr is array (Weekday range <>) of Integer;
   type Arr_Day is array (Day range <>) of Integer;

   Norm     : Arr (Mon .. Fri);          -- INDEX SUBTYPE WEEKDAY
   Norm_Day : Arr_Day (Mon .. Fri);      -- INDEX SUBTYPE DAY

begin
   Test
     ("C52101A",
      "CHECK THAT ARRAY SUBTYPE CONVERSION " &
      "APPLIED AFTER ARRAY VAL. DETERMINED");

   begin   -- ILLEGAL CASE
      Norm := (Wed .. Sun => 0);        -- ERROR: INDEX SUBTYPE

      Failed ("EXCEPTION NOT RAISED FOR INDEX SUBTYPE ERROR");

   exception
      when Constraint_Error =>
         Comment ("IMPROPER AGGREGATE BOUNDS DETECTED");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED");

   end;

   begin    -- LEGAL CASE
      Norm_Day := (Wed .. Fri => 0, Sat .. Sun => 1);
      if Norm_Day /= (0, 0, Ident_Int (0), Ident_Int (1), Ident_Int (1)) then
         Failed ("INCORRECT ASSIGNMENT IN LEGAL CASE");
      end if;

   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED ON LEGAL INDEX " & "SUBTYPE CONVERSION");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED IN LEGAL CASE");

   end;

   Result;

end C52101a;
