-- C39006D.ADA

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
-- CHECK THAT IF A FUNCTION IS USED IN A DEFAULT EXPRESSION FOR A
-- SUBPROGRAM OR FORMAL GENERIC PARAMETER, PROGRAM_ERROR IS RAISED
-- WHEN AN ATTEMPT IS MADE TO EVALUATE THE DEFAULT EXPRESSION,
-- BECAUSE THE FUNCTION'S BODY HAS NOT BEEN ELABORATED YET.

-- TBN  8/20/86

with Report; use Report;
procedure C39006d is

begin
   Test
     ("C39006D",
      "CHECK THAT IF A FUNCTION IS USED IN A DEFAULT " &
      "EXPRESSION FOR A SUBPROGRAM OR FORMAL GENERIC " &
      "PARAMETER, PROGRAM_ERROR IS RAISED WHEN AN " &
      "ATTEMPT IS MADE TO EVALUATE THE DEFAULT " &
      "EXPRESSION");
   declare
      function Fun return Integer;

      package P is
         procedure Default (A : Integer := Fun);
      end P;

      package body P is
         procedure Default (A : Integer := Fun) is
            B : Integer := 1;
         begin
            B := B + Ident_Int (A);
         end Default;
      begin
         Default (2);
         Default;
         Failed ("PROGRAM_ERROR NOT RAISED - 1");
      exception
         when Program_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 1");
      end P;

      function Fun return Integer is
      begin
         return (Ident_Int (1));
      end Fun;
   begin
      null;
   end;

   begin
      declare
         function Init_1 return Integer;

         generic
            Length : Integer := Init_1;
         package P is
            type Array1 is array (1 .. Length) of Integer;
         end P;

         package New_P1 is new P (4);
         package New_P2 is new P;

         function Init_1 return Integer is
         begin
            return (Ident_Int (2));
         end Init_1;

      begin
         Failed ("PROGRAM_ERROR NOT RAISED - 2");
      end;
   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 2");
   end;

   declare
      function Init_2 return Integer;

      Global_Int : Integer := Ident_Int (1);

      generic
      package Q is
         procedure Add1 (A : Integer := Init_2);
      end Q;

      package body Q is
         procedure Add1 (A : Integer := Init_2) is
            B : Integer;
         begin
            B := A;
         end Add1;
      begin
         if Global_Int = Ident_Int (1) then
            Add1;
            Failed ("PROGRAM_ERROR NOT RAISED - 3");
         else
            Add1 (2);
         end if;

      exception
         when Program_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 3");
      end Q;

      package New_Q is new Q;

      function Init_2 return Integer is
      begin
         return (Ident_Int (1));
      end Init_2;

   begin
      null;
   end;

   Result;
end C39006d;
