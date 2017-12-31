-- C39008A.ADA

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
-- OBJECTIVE:
--     CHECK THAT PROGRAM_ERROR IS RAISED BY AN ATTEMPT TO ACTIVATE
--     A TASK BEFORE ITS BODY HAS BEEN ELABORATED.  CHECK THE CASE IN
--     WHICH A TASK VARIABLE IS DECLARED IN A PACKAGE SPECIFICATION AND
--     THE PACKAGE BODY OCCURS BEFORE THE TASK BODY.

-- HISTORY:
--     BCB 01/21/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C39008a is

begin
   Test
     ("C39008A",
      "CHECK THAT PROGRAM_ERROR IS RAISED BY AN " &
      "ATTEMPT TO ACTIVATE A TASK BEFORE ITS BODY " &
      "HAS BEEN ELABORATED.  CHECK THE CASE IN WHICH " &
      "A TASK VARIABLE IS DECLARED IN A PACKAGE " &
      "SPECIFICATION AND THE PACKAGE BODY OCCURS " & "BEFORE THE TASK BODY");

   begin
      declare
         task type T;

         package P is
            X : T;
         end P;

         package body P is
         end P;                                  -- PROGRAM_ERROR.

         task body T is
         begin
            Comment ("TASK MESSAGE");
         end T;
      begin
         Failed ("PROGRAM_ERROR WAS NOT RAISED");
      end;
   exception
      when Program_Error =>
         Comment ("PROGRAM_ERROR WAS RAISED");
      when others =>
         Failed ("AN EXCEPTION OTHER THAN PROGRAM_ERROR WAS " & "RAISED");
   end;

   Result;
end C39008a;
