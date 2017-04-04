-- C39007A.ADA

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
-- CHECK THAT PROGRAM_ERROR IS RAISED IF AN ATTEMPT IS MADE TO INSTANTIATE A
-- GENERIC UNIT WHOSE BODY HAS NOT BEEN ELABORATED. CHECK THE FOLLOWING CASE:
--     A) A SIMPLE CASE WHERE THE GENERIC UNIT BODY OCCURS LATER IN
--        THE SAME DECLARATIVE PART.

-- TBN  9/12/86

with Report; use Report;
procedure C39007a is

begin
   Test
     ("C39007A",
      "CHECK THAT PROGRAM_ERROR IS RAISED IF AN " &
      "ATTEMPT IS MADE TO INSTANTIATE A GENERIC " &
      "UNIT WHOSE BODY HAS NOT BEEN ELABORATED, " &
      "BUT OCCURS IN THE SAME DECLARATIVE PART");

   begin
      if Equal (1, 1) then
         declare
            generic
            package P is
               A : Integer;
               procedure Assign (X : out Integer);
            end P;

            package New_P is new P;

            package body P is
               procedure Assign (X : out Integer) is
               begin
                  X := Ident_Int (1);
               end Assign;
            begin
               Assign (A);
            end P;

         begin
            null;
         end;
         Failed ("PROGRAM_ERROR WAS NOT RAISED - 1");
      end if;

   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 1");
   end;

------------------------------------------------------------------------

   begin
      if Equal (2, 2) then
         declare
            generic
            procedure Add1 (X : in out Integer);

            procedure New_Add1 is new Add1;

            procedure Add1 (X : in out Integer) is
            begin
               X := X + Ident_Int (1);
            end Add1;
         begin
            null;
         end;
         Failed ("PROGRAM_ERROR WAS NOT RAISED - 2");
      end if;

   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 2");
   end;

------------------------------------------------------------------------

   begin
      if Equal (3, 3) then
         declare
            generic
            function Init return Integer;

            function New_Init is new Init;

            function Init return Integer is
            begin
               return (Ident_Int (1));
            end Init;
         begin
            null;
         end;
         Failed ("PROGRAM_ERROR WAS NOT RAISED - 3");
      end if;

   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 3");
   end;

------------------------------------------------------------------------

   Result;
end C39007a;
