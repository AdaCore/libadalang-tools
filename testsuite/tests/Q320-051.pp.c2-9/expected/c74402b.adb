-- C74402B.ADA

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
-- CHECK THAT INITIALIZATION OF IN PARAMETERS THAT ARE OF
-- LIMITED PRIVATE TYPE IS PERMITTED.
-- (SEE ALSO 6.4.2/T1 FOR TESTS OF OTHER LIMITED TYPES.)

-- DAS  1/21/81
-- ABW  6/30/82
-- BHS  7/10/84

with Report;
procedure C74402b is

   use Report;

begin

   Test
     ("C74402B",
      "CHECK THAT INITIALIZATION OF IN PARAMETERS " &
      "OF LIMITED PRIVATE TYPE IS PERMITTED");

   declare

      package Pkg is

         type Lptype is limited private;
         Clp : constant Lptype;
         Xlp : constant Lptype;
         function Eqclp (L : in Lptype) return Boolean;
         function Eqxlp (L : in Lptype) return Boolean;

      private

         type Lptype is new Integer range 0 .. 127;
         Clp : constant Lptype := 127;
         Xlp : constant Lptype := 0;

      end Pkg;

      package body Pkg is

         function Eqclp (L : in Lptype) return Boolean is
         begin
            return (L = Clp);
         end Eqclp;

         function Eqxlp (L : in Lptype) return Boolean is
         begin
            return (L = Xlp);
         end Eqxlp;

      end Pkg;

      use Pkg;

      procedure Proc1 (Y : in Lptype := Clp) is
      begin
         if (Eqclp (Y)) then
            Failed ("LIMITED PRIVATE NOT PASSED, " & "DEFAULT CLP EMPLOYED");
         elsif (not Eqxlp (Y)) then
            Failed ("NO LIMITED PRIVATE FOUND");
         end if;
      end Proc1;

      procedure Proc2 (Y : in Lptype := Clp) is
      begin
         if (not Eqclp (Y)) then
            Failed ("DEFAULT NOT EMPLOYED");
         end if;
      end Proc2;

   begin

      Proc1 (Xlp);
      Proc2;

   end;

   Result;

end C74402b;
