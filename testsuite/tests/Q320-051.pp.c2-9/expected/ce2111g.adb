-- CE2111G.ADA

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
--     CHECK THAT A SUPPLIED MODE PARAMETER IN A RESET CHANGES
--     THE MODE OF A GIVEN FILE.  IF NO PARAMETER IS SUPPLIED
--     THE MODE REMAINS THE SAME.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     RESET FOR DIRECT FILES.

-- HISTORY:
--     DLD 08/16/82
--     SPS 11/09/82
--     JBG 03/24/83
--     EG  05/29/85
--     TBN 11/04/86  ADDED A RAISE INCOMPLETE STATEMENT WHEN FAILED
--                   IS CALLED FOR OPEN OR CREATE.
--     JLH 07/24/87  ADDED CHECKS FOR USE_ERR0R WHEN FILE IS RESET.

with Report; use Report;
with Direct_Io;

procedure Ce2111g is

   package Dir_Io is new Direct_Io (Integer);
   use Dir_Io;
   Dir_File : Dir_Io.File_Type;
   Dir_Mode : Dir_Io.File_Mode;
   Incomplete : exception;
   Var1 : Integer := 5;

begin

   Test
     ("CE2111G",
      "CHECK THAT A SUPPLIED MODE PARAMETER SETS " &
      "THE MODE OF THE GIVEN FILE APPROPRIATELY");

-- CREATE DIRECT TEST FILE

   begin
      Create (Dir_File, Inout_File, Legal_File_Name);
      Write (Dir_File, Var1);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE");
         raise Incomplete;
      when Name_Error =>
         Not_Applicable ("NAME_ERROR RAISED ON CREATE");
         raise Incomplete;
   end;

-- RESET TO DEFAULT

   begin
      Dir_Mode := Out_File;
      Reset (Dir_File);
      Dir_Mode := Mode (Dir_File);
      if Dir_Mode /= Inout_File then
         Failed ("DEFAULT RESET CHANGED MODE - DIR");
      end if;
   exception
      when Use_Error =>
         Not_Applicable ("RESET NOT SUPPORTED FOR DIR " & "INOUT_FILES");
   end;

-- RESET TO OUT_FILE

   begin
      Dir_Mode := In_File;
      Reset (Dir_File, Out_File);
      Dir_Mode := Mode (Dir_File);
      if Dir_Mode /= Out_File then
         Failed ("RESET TO OUT_FILE FAILED - DIR");
      end if;
   exception
      when Use_Error =>
         Not_Applicable
           ("RESET FROM INOUT_FILE TO OUT_FILE " &
            "NOT SUPPORTED FOR DIR FILES");
   end;

-- RESET TO IN_FILE

   begin
      Dir_Mode := Out_File;
      Reset (Dir_File, In_File);
      Dir_Mode := Mode (Dir_File);
      if Dir_Mode /= In_File then
         Failed ("RESET TO IN_FILE FAILED - DIR");
      end if;
   exception
      when Use_Error =>
         Not_Applicable
           ("RESET FROM OUT_FILE TO IN_FILE NOT " &
            "SUPPORTED FOR DIR IN_FILE");
   end;

-- RESET TO INOUT_FILE

   begin
      Dir_Mode := Out_File;
      Reset (Dir_File, Inout_File);
      Dir_Mode := Mode (Dir_File);
      if Dir_Mode /= Inout_File then
         Failed ("RESET TO INOUT_FILE FAILED - DIR");
      end if;
   exception
      when Use_Error =>
         Not_Applicable
           ("RESET FROM IN_FILE TO INOUT_FILE NOT " &
            "SUPPORTED FOR DIR INOUT_FILES");
   end;

   begin
      Delete (Dir_File);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce2111g;
