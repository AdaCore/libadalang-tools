-- C93008B.ADA

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
-- CHECK THAT AFTER CREATION OF A TASK OBJECT BY AN ALLOCATOR, ANY OPERATION
-- INVOLVING THE RESULT DELIVERED BY THE ALLOCATOR IS EXECUTED ONLY AFTER THE
-- ACTIVATION OF THE TASK HAS COMPLETED.

-- WEI  3/ 4/82
-- TBN 12/20/85 RENAMED FROM C930AJA-B.ADA. ADDED DELAY STATEMENT
--                  DURING TASK ACTIVATION.
-- RJW 4/11/86 ADDED PACKAGE DUMMY.

with Report; use Report;
procedure C93008b is

   subtype Arg is Natural range 0 .. 9;
   Spynumb : Natural := 0;

   function Finit_Pos (Digt : in Arg) return Natural is
   begin
      Spynumb := 10 * Spynumb + Digt;
      return Digt;
   end Finit_Pos;

begin

   Test
     ("C93008B", "USE OF RESULT AFTER CREATION OF " & "A TASK BY ALLOCATOR");

   Block :
   declare

      task type Tt1;

      type Att1 is access Tt1;
      type Array_Att1 is array (Natural range 2 .. 3) of Att1;
      My_Array    : Array_Att1;
      Pointer_Tt1 : Att1;

      task body Tt1 is
         package Dummy is
         end Dummy;

         package body Dummy is
         begin
            delay 2.0;
            declare
               Idummy1 : Natural := Finit_Pos (1);
            begin
               null;
            end;
         end Dummy;
      begin
         null;
      end Tt1;

   begin

      My_Array    := (2 => new Tt1, 3 => null);  -- TASK ACTIVATED NOW.
      Pointer_Tt1 := My_Array (Finit_Pos (2));

      My_Array (Finit_Pos (3)) := Pointer_Tt1;

      if Spynumb /= 123 then
         if Spynumb = 132 or Spynumb = 13 or Spynumb = 12 or Spynumb = 1 or
           Spynumb = 0 then
            Failed ("TASK ACTIVATION RIGHT IN TIME, " & "BUT OTHER ERROR");
         else
            Failed
              ("RESULT OF ALLOCATOR ACCESSED BEFORE " &
               "TASK ACTIVATION HAS COMPLETED");
         end if;
         Comment ("ACTUAL ORDER WAS:" & Integer'Image (Spynumb));
      end if;
   end Block;

   Result;

end C93008b;
