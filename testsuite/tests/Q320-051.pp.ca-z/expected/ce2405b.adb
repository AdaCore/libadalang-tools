-- CE2405B.ADA

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
--     CHECK THAT READ RAISES END_ERROR WHEN THE CURRENT READ POSITION
--     IS GREATER THAN THE END POSITION.  ALSO CHECK THAT END_OF_FILE
--     CORRECTLY DETECTS THE END OF A DIRECT FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION WITH INOUT_FILE MODE AND OPENING OF IN_FILE MODE.

-- HISTORY:
--     SPS 09/28/82
--     JBG 02/22/84  CHANGE TO .ADA TEST
--     EG  05/16/85
--     GMT 08/03/87  ADDED CODE TO CHECK THAT END_OF_FILE WORKS, AND
--                   ADDED CODE TO PREVENT SOME EXCEPTION PROPAGATION.

with Report; use Report;
with Direct_Io;

procedure Ce2405b is
begin
   Test
     ("CE2405B",
      "CHECK THAT END_ERROR IS RAISED BY READ AT THE " &
      "END OF A FILE AND THAT END_OF_FILE CORRECTLY " &
      "DETECTS THE END OF A DIRECT_IO FILE");
   declare
      package Dir is new Direct_Io (Character);
      use Dir;
      Ft : File_Type;
      Ch : Character;
      Incomplete : exception;
   begin

      -- CREATE AND INITIALIZE FILE

      begin
         Create (Ft, Inout_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR WAS " & "RAISED ON CREATE - 1");
            raise Incomplete;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON CREATE - 2");
            raise Incomplete;
      end;

      begin

         Write (Ft, 'C');
         Write (Ft, 'X');

         -- BEGIN TEST

         if not End_Of_File (Ft) then
            Failed ("END_OF_FILE RETURNED INCORRECT " & "BOOLEAN VALUE - 3");
         end if;

         begin
            Read (Ft, Ch);
            Failed ("END_ERROR NOT RAISED ON READ - 4");
         exception
            when End_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED ON READ - 5");
         end;

         Write (Ft, 'E');

         begin
            Read (Ft, Ch);
            Failed ("END_ERROR NOT RAISED ON READ - 6");
         exception
            when End_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED ON READ - 7");
         end;

      end;

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED ON OPEN - 8");
            raise Incomplete;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON OPEN - 9");
            raise Incomplete;
      end;

      declare
         Count_Nbr_Of_Reads : Natural  := 0;
         Expected_Count     : constant := 3;
      begin
         loop
            if End_Of_File (Ft) then
               exit;
            else
               Read (Ft, Ch);
               Count_Nbr_Of_Reads := Count_Nbr_Of_Reads + 1;
            end if;
         end loop;

         if Count_Nbr_Of_Reads /= Expected_Count then
            Failed
              ("THE BAD VALUE FOR COUNT_NBR_OF_READS " &
               "IS " &
               Natural'Image (Count_Nbr_Of_Reads));
         end if;

      end;

      begin
         Delete (Ft);
      exception
         when Use_Error =>
            null;
      end;

   exception

      when Incomplete =>
         null;

   end;

   Result;

end Ce2405b;
