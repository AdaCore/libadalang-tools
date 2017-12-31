-- CE2401F.ADA

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
--     CHECK THAT READ (WITH AND WITHOUT PARAMETER FROM), WRITE (WITH
--     AND WITHOUT PARAMETER TO), SET_INDEX, INDEX, SIZE, AND
--     END_OF_FILE ARE SUPPORTED FOR DIRECT FILES WITH ELEMENT_TYPE
--     PRIVATE.

-- APPLICABILITY CRITERIA:
--
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION WITH INOUT_FILE MODE AND OPENING WITH IN_FILE MODE FOR
--     DIRECT FILES.

-- HISTORY:
--     ABW 08/18/82
--     SPS 09/15/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGE TO .ADA TEST
--     EG  11/19/85  CORRECT SO TEST CAN HANDLE IMPLEMENTATION WITH
--                   POSITIVE_COUNT'LAST=1; COVER POSSIBILITY OF CREATE
--                   RAISING USE_ERROR; ENSURE RESET DOESN'T RAISE
--                   EXCEPTION IF CREATE FAILS; CHECK THAT WE CAN READ
--                   DATA THAT HAS BEEN WRITTEN.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/11/87  ISOLATED EXCEPTIONS.

with Report; use Report;
with Direct_Io;

procedure Ce2401f is

   End_Subtest : exception;

begin

   Test
     ("CE2401F",
      "CHECK THAT READ, WRITE, SET_INDEX, " &
      "INDEX, SIZE, AND END_OF_FILE ARE " &
      "SUPPORTED FOR DIRECT FILES WITH " & "ELEMENT_TYPE PRIVATE");

   declare

      package Pkg is
         type Priv is private;
         function Assign return Priv;
      private
         type Priv is new Integer;
      end Pkg;

      use Pkg;

      package Dir_Prv is new Direct_Io (Priv);
      use Dir_Prv;
      File_Prv : File_Type;

      package body Pkg is
         function Assign return Priv is
         begin
            return (16);
         end Assign;
      begin
         null;
      end Pkg;

   begin
      begin
         Create (File_Prv, Inout_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("USE_ERROR | NAME_ERROR RAISED " & "ON CREATE - PRIVATE");
            raise End_Subtest;
         when others =>
            Failed ("UNEXPECTED ERROR RAISED ON " & "CREATE - PRIVATE");
            raise End_Subtest;
      end;

      begin

         declare

            Prv, Item_Prv : Priv;
            One_Prv       : Positive_Count := 1;
            Two_Prv       : Positive_Count := 2;

         begin

            Prv := Assign;

            begin
               Write (File_Prv, Prv);
            exception
               when others =>
                  Failed ("EXCEPTION RAISED ON WRITE FOR " & "PRIVATE - 1");
            end;

            begin
               Write (File_Prv, Prv, Two_Prv);
            exception
               when others =>
                  Failed ("EXCEPTION RAISED ON WRITE FOR " & "PRIVATE - 2");
            end;

            begin
               if Size (File_Prv) /= Two_Prv then
                  Failed ("SIZE FOR TYPE PRIVATE");
               end if;
               if not End_Of_File (File_Prv) then
                  Failed ("WRONG END_OF_FILE VALUE FOR " & "PRIVATE TYPE");
               end if;

               Set_Index (File_Prv, One_Prv);

               if Index (File_Prv) /= One_Prv then
                  Failed ("WRONG INDEX VALUE FOR PRIVATE " & "TYPE");
               end if;
            end;

            Close (File_Prv);

            begin
               Open (File_Prv, In_File, Legal_File_Name);
            exception
               when Use_Error =>
                  Not_Applicable ("OPEN FOR IN_FILE NOT " & "SUPPORTED");
                  raise End_Subtest;
            end;

            begin
               Read (File_Prv, Item_Prv);
               if Item_Prv /= Prv then
                  Failed ("INCORRECT PRIVATE TYPE VALUE " & "READ - 1");
               end if;
            exception
               when others =>
                  Failed ("READ WITHOUT FROM FOR " & "PRIVATE TYPE");
            end;

            begin
               Read (File_Prv, Item_Prv, One_Prv);
               if Item_Prv /= Prv then
                  Failed ("INCORRECT PRIVATE TYPE VALUE " & "READ - 2");
               end if;
            exception
               when others =>
                  Failed ("READ WITH FROM FOR " & "PRIVATE TYPE");
            end;
         end;

         begin
            Delete (File_Prv);
         exception
            when Use_Error =>
               null;
         end;

      end;

   exception
      when End_Subtest =>
         null;
   end;

   Result;

end Ce2401f;
