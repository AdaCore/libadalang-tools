-- CC3011A.ADA

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
-- CHECK THAT SUBPROGRAMS THAT WOULD HAVE THE SAME SPECIFICATION AFTER GENERIC
-- INSTANTIATION MAY BE DECLARED IN THE SAME DECLARATIVE PART, AND THAT CALLS
-- WITHIN THE INSTANTIATED UNIT ARE UNAMBIGUOUS. CHECK THAT CALLS FROM OUTSIDE
-- THE UNIT ARE UNAMBIGUOUS IF FORMAL PARAMETER NAMES ARE USED OR IF ONLY ONE
-- OF THE EQUIVALENT PROGRAMS APPEARS IN THE VISIBLE PART OF THE PACKAGE.

-- DAT 9/18/81
-- SPS 10/19/82

with Report; use Report;

procedure Cc3011a is
begin
   Test
     ("CC3011A",
      "CHECK SUBPROGRAMS IN GENERIC PACKAGES WITH SAME" &
      " SPECIFICATION AFTER GENERIC PARAMETER SUBSTITUTION");

   declare
      C : Integer := 0;

      generic
         type S is (<>);
         type T is private;
         type U is range <>;
         Vt : T;
      package Pkg is
         procedure P1 (X : S);
      private
         procedure P1 (X : T);
         Vs : S := S'First;
         Vu : U := U'First;
      end Pkg;

      generic
         type S is (<>);
         type T is range <>;
      package Pp is
         procedure P1 (D : S);
         procedure P1 (X : T);
      end Pp;

      package body Pkg is
         procedure P1 (X : S) is
         begin
            C := C + 1;
         end P1;
         procedure P1 (X : T) is
         begin
            C := C + 2;
         end P1;
         procedure P1 (X : U) is
         begin
            C := C + 4;
         end P1;
      begin
         C := 0;
         P1 (Vs);
         if C /= Ident_Int (1) then
            Failed ("WRONG P1 CALLED -S");
         end if;
         C := 0;
         P1 (Vt);
         if C /= Ident_Int (2) then
            Failed ("WRONG P1 CALLED -T");
         end if;
         C := 0;
         P1 (Vu);
         if C /= Ident_Int (4) then
            Failed ("WRONG P1 CALLED -U");
         end if;
         C := 0;
      end Pkg;

      package body Pp is
         procedure P1 (D : S) is
         begin
            C := C + 3;
         end P1;
         procedure P1 (X : T) is
         begin
            C := C + 5;
         end P1;
      begin
         null;
      end Pp;

      package Np is new Pkg (Integer, Integer, Integer, 7);
      package Npp is new Pp (Integer, Integer);
   begin
      Np.P1 (4);
      if C /= Ident_Int (1) then
         Failed ("INCORRECT OVERLOADING ON FORMAL TYPES");
      end if;
      C := 0;
      Npp.P1 (D => 3);
      if C /= Ident_Int (3) then
         Failed ("INCORRECT CALL TO P1 WITH D PARAMETER");
      end if;
      C := 0;
      Npp.P1 (X => 7);
      if C /= Ident_Int (5) then
         Failed ("INCORRECT CALL TO P1 WITH X PARAMETER");
      end if;
   end;

   Result;
end Cc3011a;
