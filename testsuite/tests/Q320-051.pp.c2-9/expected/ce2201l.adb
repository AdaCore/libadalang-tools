-- CE2201L.ADA

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
--     CHECK THAT READ, WRITE, AND END_OF_FILE ARE SUPPORTED FOR
--     SEQUENTIAL FILES WITH ELEMENT TYPE FIXED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     JLH 08/03/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Sequential_Io;

procedure Ce2201l is
begin

   Test
     ("CE2201L",
      "CHECK THAT READ, WRITE, AND END_OF_FILE " &
      "ARE SUPPORTED FOR SEQUENTIAL FILES - FIXED");

   declare
      type Fix is delta 0.5 range -10.0 .. 255.0;
      package Seq_Fix is new Sequential_Io (Fix);
      use Seq_Fix;
      File_Fix : File_Type;
      Incomplete : exception;
      Fx       : Fix := -8.5;
      Item_Fix : Fix;
   begin
      begin
         Create (File_Fix, Out_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("CREATE OF SEQUENTIAL FILE WITH " &
               "MODE OUT_FILE NOT SUPPORTED");
            raise Incomplete;
      end;

      Write (File_Fix, Fx);
      Close (File_Fix);

      begin
         Open (File_Fix, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("OPEN OF SEQUENTIAL FILE WITH " & "MODE IN_FILE NOT SUPPORTED");
            raise Incomplete;
      end;

      if End_Of_File (File_Fix) then
         Failed ("WRONG END_OF_FILE VALUE FOR FIXED POINT");
      end if;

      Read (File_Fix, Item_Fix);

      if not End_Of_File (File_Fix) then
         Failed ("END OF FILE NOT TRUE - FIXED");
      end if;

      if Item_Fix /= -8.5 then
         Failed ("READ WRONG VALUE - STRING");
      end if;

      begin
         Delete (File_Fix);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce2201l;
