-- CE3108B.ADA

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
--     CHECK THAT THE NAME RETURNED BY THE NAME FUNCTION CAN BE USED
--     IN A SUBSEQUENT OPEN.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     DLD 08/11/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/16/85
--     GMT 08/17/87  REMOVED UNNECESSARY CODE AND ADDED A CHECK FOR
--                   USE_ERROR ON DELETE.

with Text_Io; use Text_Io;
with Report;  use Report;

procedure Ce3108b is

   type Acc_Str is access String;

   Txt_File      : File_Type;
   Txt_File_Name : Acc_Str;
   Dir_File_Name : Acc_Str;
   Var           : String (1 .. 2);
   Last          : Integer;
   Incomplete : exception;

begin

   Test
     ("CE3108B",
      "CHECK THAT THE NAME RETURNED BY THE NAME-" &
      "FUNCTION CAN BE USED IN A SUBSEQUENT OPEN");

   -- CREATE TEST FILES

   begin
      begin
         Create (Txt_File, Out_File, Legal_File_Name);
      exception
         when Name_Error =>
            Not_Applicable ("NAME_ERROR RAISED ON CREATE - 1");
            raise Incomplete;
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED ON CREATE - 2");
            raise Incomplete;
      end;

      begin
         Put (Txt_File, "14");
         Txt_File_Name := new String'(Name (Txt_File));
         Close (Txt_File);

         -- ATTEMPT TO RE-OPEN TEXT TEST FILE USING RETURNED NAME VALUE

         begin
            Open (Txt_File, In_File, Txt_File_Name.all);
         exception
            when Use_Error =>
               Not_Applicable ("USE_ERROR ON RE-OPEN - 3");
               raise Incomplete;
         end;

         Get (Txt_File, Var);
         if Var /= "14" then
            Failed ("WRONG DATA RETURNED FROM READ - 4");
         end if;

         -- CLOSE AND DELETE TEST FILES

         begin
            Delete (Txt_File);
         exception
            when Use_Error =>
               null;
         end;
      end;
   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3108b;
