-- C91006A.ADA

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
-- CHECK THAT IN A TASK SPECIFICATION ENTRY DECLARATIONS ARE ELABORATED
-- WHEN THE SPECIFICATION IS ELABORATED, AND IN TEXTUAL ORDER.

-- WEI  3/04/82
-- BHS  7/13/84
-- TBN 12/17/85     RENAMED FROM C910AHA-B.ADA;
--                  ADDED DECLARATIONS OF FIRST AND LAST.
-- PWB  5/15/86     MOVED DECLARATIONS OF FIRST, TASK T1, AND LAST
--                  INTO A DECLARE/BEGIN/END BLOCK.

with Report; use Report;
procedure C91006a is

   subtype Arg is Natural range 0 .. 9;
   Index   : Integer range 0 .. 5 := 0;
   Spynumb : String (1 .. 5)      := (1 .. 5 => ' ');

   function Finit_Pos (Digt : in Arg) return Natural is
      Temp : String (1 .. 2);
   begin
      Temp            := Arg'Image (Digt);
      Index           := Index + 1;
      Spynumb (Index) := Temp (2);
      return Digt;
   end Finit_Pos;

begin
   Test
     ("C91006A",
      "CHECK THAT IN A TASK SPEC, ELABORATION IS IN " & "TEXTUAL ORDER");
   declare

      First : Integer := Finit_Pos (1);

      task T1 is
         entry E2 (Natural range 1 .. Finit_Pos (2));
         entry E3 (Natural range 1 .. Finit_Pos (3));
         entry E4 (Natural range 1 .. Finit_Pos (4));
      end T1;

      Last : Integer := Finit_Pos (5);

      task body T1 is
      begin
         null;
      end T1;

   begin
      null;
   end;

   if Spynumb /= "12345" then
      Failed ("TASK SPEC T1 NOT ELABORATED IN TEXTUAL ORDER");
      Comment ("ACTUAL ORDER WAS: " & Spynumb);
   end if;

   Result;

end C91006a;
