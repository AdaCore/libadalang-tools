-- CC3305D.ADA

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
-- CHECK THAT WHEN A GENERIC FORMAL TYPE IS A SCALAR TYPE, THE BOUNDS OF THE
-- ACTUAL PARAMETER ARE USED WITHIN THE INSTANTIATED UNIT.

-- CHECK WHEN THE SCALAR TYPE IS DEFINED BY DELTA <>.

-- SPS 7/15/82

with Report; use Report;

procedure Cc3305d is
begin

   Test
     ("CC3305D",
      "TEST THE BOUNDS OF GENERIC FORMAL SCALAR " &
      "TYPES OF THE FORM DELTA <>");

   declare
      type Fx is delta 0.1 range 1.0 .. 3.0;

      generic
         type Gft is delta <>;
      package Pk is
      end Pk;

      package body Pk is
      begin
         for I in Ident_Int (0) .. Ident_Int (4) loop
            Comment ("START OF ITERATION");
            declare
               Var : Gft;
            begin
               Var := Gft (I);
               if I = Ident_Int (0) or I = Ident_Int (4) then
                  Failed ("CONSTRAINT_ERROR NOT RAISED");
               end if;
            exception
               when Constraint_Error =>
                  if I /= Ident_Int (0) and I /= Ident_Int (4) then
                     Failed ("CONSTRAINT_ERROR RAISED " & "INAPPROPRIATELY");
                  end if;
            end;
         end loop;
      end Pk;

   begin

      declare
         package Np is new Pk (Fx);
      begin
         null;
      end;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED ON INSTANTIATION");
   end;

   Result;
end Cc3305d;
