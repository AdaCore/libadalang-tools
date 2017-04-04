-- C95095C.ADA

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
-- CHECK THAT OVERLOADED ENTRY DECLARATIONS
-- ARE PERMITTED IN WHICH THERE IS A MINIMAL
-- DIFFERENCE BETWEEN THE DECLARATIONS.

--     (C) THE BASE TYPE OF A PARAMETER IS DIFFERENT FROM THAT
--         OF THE CORRESPONDING ONE.

-- JWC 7/24/85

with Report; use Report;
procedure C95095c is

begin
   Test ("C95095C", "ENTRY OVERLOADING WITH " & "MINIMAL DIFFERENCES");

   --------------------------------------------------

   -- THE BASE TYPE OF ONE PARAMETER IS
   -- DIFFERENT FROM THAT OF THE CORRESPONDING
   -- ONE.

   declare

      type Newint is new Integer;

      I, J, K : Integer         := 0;
      N       : Newint;
      S       : String (1 .. 2) := "12";

      task T is
         entry E (I1 : Integer; N1 : out Newint; I2 : in out Integer);
         entry E (I1 : Integer; N1 : out Integer; I2 : in out Integer);
      end T;

      task body T is
      begin
         loop
            select
               accept E (I1 : Integer; N1 : out Newint; I2 : in out Integer) do
                  S (1) := 'A';
                  N1    := 0; -- THIS VALUE IS IRRELEVENT.
               end E;
            or
               accept E
                 (I1 :        Integer;
                  N1 :    out Integer;
                  I2 : in out Integer) do
                  S (2) := 'B';
                  N1    := 0; -- THIS VALUE IS IRRELEVENT.
               end E;
            or
               terminate;
            end select;
         end loop;
      end T;

   begin
      T.E (I, N, K);
      T.E (I, J, K);

      if S /= "AB" then
         Failed
           ("ENTRIES DIFFERING ONLY BY " &
            "THE BASE TYPE OF A PARAMETER " &
            "CAUSED CONFUSION");
      end if;
   end;

   --------------------------------------------------

   Result;
end C95095c;
