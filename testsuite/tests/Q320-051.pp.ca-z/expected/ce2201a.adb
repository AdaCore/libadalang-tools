-- CE2201A.ADA

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
--     SEQUENTIAL FILES WITH ELEMENT_TYPE STRING.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     ABW 08/16/82
--     SPS 11/09/82
--     JBG 01/05/83
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 07/28/87  REMOVED DEPENDENCE ON SUPPORT OF RESET.

with Report; use Report;
with Sequential_Io;

procedure Ce2201a is

begin

   Test
     ("CE2201A",
      "CHECK THAT READ, WRITE, AND " & "END_OF_FILE ARE SUPPORTED FOR " &
      "SEQUENTIAL FILES - STRING TYPE");

   declare
      subtype Strng is String (1 .. 12);
      package Seq_Str is new Sequential_Io (Strng);
      use Seq_Str;
      File_Str : File_Type;
      Incomplete : exception;
      Str      : Strng := "TEXT OF FILE";
      Item_Str : Strng;
   begin
      begin
         Create (File_Str, Out_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("CREATE OF SEQUENTIAL FILE WITH " &
               "MODE OUT_FILE NOT SUPPORTED");
            raise Incomplete;
      end;

      Write (File_Str, Str);
      Close (File_Str);

      begin
         Open (File_Str, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("OPEN OF SEQUENTIAL FILE WITH " & "MODE IN_FILE NOT SUPPORTED");
            raise Incomplete;
      end;

      if End_Of_File (File_Str) then
         Failed ("WRONG END_OF_FILE VALUE FOR TYPE STRING");
      end if;

      Read (File => File_Str, Item => Item_Str);

      if Item_Str /= Strng (Ident_Str ("TEXT OF FILE")) then
         Failed ("READ WRONG VALUE - STRING");
      end if;

      if not End_Of_File (File_Str) then
         Failed ("END OF FILE NOT TRUE - STRING");
      end if;

      begin
         Delete (File_Str);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce2201a;
