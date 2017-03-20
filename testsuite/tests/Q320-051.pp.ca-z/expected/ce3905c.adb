-- CE3905C.ADA

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
--     CHECK THAT GET FOR ENUMERATION TYPES RAISES DATA_ERROR WHEN THE
--     ELEMENT RETRIEVED IS NOT OF THE TYPE EXPECTED OR IS OUT OF THE
--     RANGE OF A SUBTYPE.  ALSO CHECK THAT CONSTRAINT_ERROR IS RAISED
--     IF THE VALUE READ IS OUT OF RANGE OF THE ITEM PARAMETER, BUT
--     WITHIN THE RANGE OF THE INSTANTIATED TYPE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/08/82
--     SPS 12/14/82
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/16/87  REMOVED DEPENDENCE ON RESET AND CORRECTED
--                   EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3905c is
   Incomplete : exception;

begin

   Test
     ("CE3905C",
      "CHECK THAT GET FOR ENUMERATION TYPES RAISES " &
      "DATA_ERROR WHEN THE ELEMENT RETRIEVED IS NOT " &
      "OF THE TYPE EXPECTED OR IS OUT OF THE RANGE " &
      "OF A SUBTYPE.  ALSO CHECK THAT " &
      "CONSTRAINT_ERROR IS RAISED IF THE VALUE READ " &
      "IS OUT OF RANGE OF THE ITEM PARAMETER, BUT " &
      "WITHIN THE RANGE OF THE INSTANTIATED TYPE");

   declare
      Ft : File_Type;
      type Color is (Red, Blue, Yellow, White, Orange, Green, Purple, Black);
      subtype P_Color is Color range Red .. Yellow;
      Crayon : Color   := Black;
      Paint  : P_Color := Blue;
      St     : String (1 .. 2);
      package Color_Io is new Enumeration_Io (Color);
      use Color_Io;
   begin

-- CREATE AND INITIALIZE DATA FILE

      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      Put (Ft, "BROWN");
      New_Line (Ft);
      Put (Ft, "ORANGE");
      New_Line (Ft);
      Put (Ft, "GREEN");
      New_Line (Ft);
      Put (Ft, "WHITE");
      New_Line (Ft);
      Put (Ft, "WHI");
      New_Line (Ft);
      Put (Ft, "TE");
      New_Line (Ft);
      Put (Ft, "RED");

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED; OPEN WITH " & "IN_FILE MODE");
            raise Incomplete;
      end;

-- START TEST

      begin
         Get (Ft, Crayon);                  -- BROWN
         Failed ("DATA_ERROR NOT RAISED - 1");
      exception
         when Data_Error =>
            if Crayon /= Black then
               Failed ("ITEM CRAYON AFFECTED - 1");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 1");
      end;

      begin
         Get (Ft, Paint);                   -- ORANGE
         Failed ("CONSTRAINT_ERROR NOT RAISED");
      exception
         when Constraint_Error =>
            if Paint /= Blue then
               Failed ("ITEM PAINT AFFECTED - 2");
            end if;
         when Data_Error =>
            Failed ("DATA_ERROR RAISED FOR ITEM SUBTYPE");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 2");
      end;

      declare
         package P_Color_Io is new Enumeration_Io (P_Color);
         use P_Color_Io;
      begin
         begin
            P_Color_Io.Get (Ft, Paint);   -- GREEN
            Failed ("DATA_ERROR NOT RAISED - 3");
         exception
            when Data_Error =>
               if Paint /= Blue then
                  Failed ("ITEM PAINT AFFECTED - 3");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 3");
         end;

         begin
            P_Color_Io.Get (Ft, Paint);   -- WHITE
            Failed ("DATA_ERROR NOT RAISED - 3A");
         exception
            when Data_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 3A");
         end;
      end;

      begin
         Get (Ft, Crayon);                  -- WHI
         Failed ("DATA_ERROR NOT RAISED - 4");
      exception
         when Data_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 4");
      end;

      Get (Ft, St);                 -- TE

      Get (Ft, Crayon);                  -- RED
      if Crayon /= Red then
         Failed
           ("READING NOT CONTINUED CORRECTLY AFTER" & "DATA_ERROR EXCEPTION");
      end if;

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

end Ce3905c;
