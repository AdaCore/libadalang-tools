-- CE2406A.ADA

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
--     FOR A DIRECT ACCESS FILE, CHECK THAT AFTER A READ, THE CURRENT
--     READ POSITION IS INCREMENTED BY ONE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT_IO FILES.

-- HISTORY:
--     ABW 08/20/82
--     SPS 09/16/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGE TO .ADA TEST.
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 08/05/87  REMOVED DEPENDENCE ON RESET AND ADDED CHECK FOR
--                   USE_ERROR ON DELETE.

with Report; use Report;
with Direct_Io;

procedure Ce2406a is

   package Dir is new Direct_Io (Integer);
   use Dir;
   File1                : File_Type;
   Int                  : Integer := Ident_Int (18);
   Bool                 : Boolean := Ident_Bool (True);
   Int_Item1, Int_Item2 : Integer;
   Incomplete : exception;

begin

   Test
     ("CE2406A",
      "CHECK THAT READ POSITION IS INCREMENTED " & "BY ONE AFTER A READ");

   -- CREATE AND INITIALIZE FILE1

   begin

      begin
         Create (File1, Inout_File, Legal_File_Name);
      exception
         when Name_Error | Use_Error =>
            Not_Applicable
              ("NAME_ERROR | USE_ERROR RAISED " & "ON CREATE - 1");
            raise Incomplete;
         when others =>
            Failed ("WRONG EXCEPTION RAISED ON CREATE - 2");
            raise Incomplete;
      end;

      begin
         Write (File1, Int);
         Write (File1, 26);
         Write (File1, 12);
         Write (File1, 19);
         Write (File1, Int);
         Write (File1, 3);

         -- BEGIN TEST

         Close (File1);
         begin
            Open (File1, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable ("USE_ERROR RAISED ON" & "OPEN - 3");
               raise Incomplete;
            when others =>
               Failed ("WRONG EXCEPTION RAISED ON " & "OPEN - 4");
               raise Incomplete;
         end;

         if Index (File1) /= Positive_Count (Ident_Int (1)) then
            Failed ("INITIAL INDEX VALUE INCORRECT - 5");
         else
            Read (File1, Int_Item1);
            if Index (File1) /= Positive_Count (Ident_Int (2)) then
               Failed ("INDEX VALUE NOT INCREMENTED - 6");
            else
               if Int_Item1 /= Ident_Int (18) then
                  Failed ("READ INCORRECT VALUE - 7");
               end if;
               Read (File1, Int_Item1, 4);
               if Index (File1) /= Positive_Count (Ident_Int (5)) then
                  Failed
                    ("INDEX VALUE NOT INCREMENTED " &
                     "WHEN TO IS SPECIFIED - 8");
               else
                  if Int_Item1 /= Ident_Int (19) then
                     Failed ("READ INCORRECT VALUE - 9");
                  end if;
                  Read (File1, Int_Item1);
                  if Index (File1) /= Positive_Count (Ident_Int (6)) then
                     Failed
                       ("INDEX VALUE NOT " & "INCREMENTED WHEN " &
                        "LAST - 10");
                  elsif Int_Item1 /= Ident_Int (18) then
                     Failed ("READ INCORRECT " & "IN_FILE VALUE - 11");
                  end if;
               end if;
            end if;
         end if;

         Close (File1);
         begin
            Open (File1, Inout_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable ("USE_ERROR RAISED ON " & "OPEN - 12");
               raise Incomplete;
            when others =>
               Failed ("WRONG EXCEPTION RAISED ON " & "OPEN - 13");
               raise Incomplete;
         end;

         if Index (File1) /= Positive_Count (Ident_Int (1)) then
            Failed ("INITIAL INDEX VALUE INCORRECT - 14");
         else
            Read (File1, Int_Item2);
            if Index (File1) /= Positive_Count (Ident_Int (2)) then
               Failed ("INDEX VALUE NOT INCREMENTED - 15");
            else
               if Int_Item2 /= Ident_Int (18) then
                  Failed ("READ INCORRECT VALUE - 16");
               end if;
               Read (File1, Int_Item2, 4);
               if Index (File1) /= Positive_Count (Ident_Int (5)) then
                  Failed
                    ("INDEX VALUE NOT INCREMENTED " &
                     "WHEN TO IS SPECIFIED - 17");
               else
                  if Int_Item2 /= Ident_Int (19) then
                     Failed ("INCORRECT VALUE - 18");
                  end if;
                  Read (File1, Int_Item2);
                  if Index (File1) /= Positive_Count (Ident_Int (6)) then
                     Failed
                       ("INDEX VALUE NOT " & "INCREMENTED WHEN " &
                        "LAST - INOUT_FILE - 19");
                  elsif Int_Item2 /= Ident_Int (18) then
                     Failed ("READ INCORRECT " & "INOUT_FILE VALUE - 20");
                  end if;
               end if;
            end if;
         end if;

         begin
            Delete (File1);
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

end Ce2406a;
