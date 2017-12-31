-- CE2201M.ADA

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
--     CHECK THAT READ, WRITE, AND END_OF_FILE ARE SUPPORTED
--     FOR SEQUENTIAL FILES WITH ELEMENT_TYPE RECORD WITHOUT
--     DISCRIMINANTS.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT SEQUENTIAL FILES WITH ELEMENT_TYPE RECORD WITHOUT
--     DISCRIMINANTS.

-- HISTORY:
--     ABW 08/17/82
--     SPS 09/15/82
--     SPS 11/09/82
--     JBG 05/02/83
--     EG  05/08/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 07/28/87  REMOVED THE DEPENDENCE OF RESET BEING SUPPORTED
--                   AND CREATED EXTERNAL FILES RATHER THAN TEMPORARY
--                   FILES.

with Report; use Report;
with Sequential_Io;

procedure Ce2201m is

begin

   Test
     ("CE2201M",
      "CHECK THAT READ, WRITE, AND " & "END_OF_FILE ARE SUPPORTED FOR " &
      "SEQUENTIAL FILES - RECORD WITHOUT " & "DISCRIMINANTS");

   declare
      type Rec is record
         One : Integer;
         Two : Integer;
      end record;
      package Seq_Rec is new Sequential_Io (Rec);
      use Seq_Rec;
      File_Rec : File_Type;
      Incomplete : exception;
      Rec1      : Rec := (One => 18, Two => 36);
      Item_Rec1 : Rec;
   begin

      begin
         Create (File_Rec, Out_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("CREATE OF SEQUENTIAL FILE WITH " &
               "MODE OUT_FILE NOT SUPPORTED");
            raise Incomplete;
      end;

      Write (File_Rec, Rec1);
      Close (File_Rec);

      begin
         Open (File_Rec, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("OPEN OF SEQUENTIAL FILE WITH " & "MODE IN_FILE NOT SUPPORTED");
            raise Incomplete;
      end;

      if End_Of_File (File_Rec) then
         Failed ("WRONG END_OF_FILE VALUE FOR TYPE RECORD");
      end if;

      Read (File_Rec, Item_Rec1);

      if Item_Rec1 /= (18, Ident_Int (36)) then
         Failed ("READ WRONG VALUE - RECORD");
      end if;

      if not End_Of_File (File_Rec) then
         Failed ("END OF FILE NOT TRUE - RECORD");
      end if;

      begin
         Delete (File_Rec);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce2201m;
