-- CE3901A.ADA

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
--     CHECK THAT GET AND PUT FOR ENUMERATED TYPES RAISE STATUS ERROR
--     IF THE FILE IS NOT OPEN.

-- HISTORY:
--     SPS 10/07/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     DWC 09/16/87  ADDED AN ATTEMPT TO CREATE A FILE AND THEN
--                   RETESTED OBJECTIVE.
--     BCB 10/03/90  ADDED NAME_ERROR AS A CHOICE TO THE EXCEPTION
--                   HANDLER FOR CREATE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3901a is
begin

   Test
     ("CE3901A",
      "CHECK THAT GET AND PUT FOR ENUMERATED TYPES " &
      "RAISE STATUS ERROR IF THE FILE IS NOT OPEN.");

   declare
      type Color is (Red, Blue, Green, Orange, Yellow);
      Ft : File_Type;
      package Color_Io is new Enumeration_Io (Color);
      use Color_Io;
      X : Color;
   begin
      begin
         Put (Ft, Red);
         Failed ("STATUS_ERROR NOT RAISED - PUT - 1");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PUT - 1");
      end;

      begin
         Get (Ft, X);
         Failed ("STATUS_ERROR NOT RAISED - GET - 1");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET - 1");
      end;

      begin
         Create (Ft, Out_File, Legal_File_Name);  -- THIS IS JUST
         Close (Ft);                   -- AN ATTEMPT TO CREATE A
      exception                          -- FILE.  OBJECTIVE IS MET
         when Use_Error                -- EITHER WAY.
         | Name_Error =>
            null;
      end;

      begin
         Put (Ft, Red);
         Failed ("STATUS_ERROR NOT RAISED - PUT - 2");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PUT - 2");
      end;

      begin
         Get (Ft, X);
         Failed ("STATUS_ERROR NOT RAISED - GET - 2");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET - 2");
      end;
   end;

   Result;

end Ce3901a;
