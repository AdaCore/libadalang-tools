-- CE2409A.ADA

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
--     FOR DIRECT ACCESS FILES, CHECK THAT A WRITE TO A POSITION
--     GREATER THAN THE CURRENT END POSITION CAUSES THE WRITE
--     POSITION AND THE FILE SIZE TO BE INCREMENTED.

--          1) CHECK FILES OF MODE INOUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH INOUT_FILE MODE FOR DIRECT FILES.

-- HISTORY:
--     ABW 08/27/82
--     SPS 11/09/82
--     SPS 03/18/83
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 08/05/87  REVISED EXCEPTION HANDLING, ADDED CHECK FOR WRITE
--                   USING TO, AND MOVED OUT_FILE CASE TO CE2409B.ADA.

with Report; use Report;
with Direct_Io;

procedure Ce2409a is

   package Dir is new Direct_Io (Integer);
   use Dir;
   File1 : File_Type;
   Incomplete : exception;

begin

   Test
     ("CE2409A",
      "CHECK THAT WRITE POSITION AND " &
      "SIZE ARE INCREMENTED CORRECTLY FOR " &
      "DIR FILES OF MODE INOUT_FILE");

   begin
      Create (File1, Inout_File, Legal_File_Name);
   exception
      when Use_Error | Name_Error =>
         Not_Applicable
           ("CREATE WITH INOUT_FILE MODE NOT " &
            "SUPPORTED FOR DIR FILES - 1");
         raise Incomplete;
   end;

   declare
      Int      : Integer        := Ident_Int (18);
      Two_C    : Count          := Count (Ident_Int (2));
      Three_Pc : Positive_Count := Positive_Count (Ident_Int (3));
      Five_C   : Count          := Count (Ident_Int (5));
      Five_Pc  : Positive_Count := Positive_Count (Ident_Int (5));
      Six_Pc   : Positive_Count := Positive_Count (Ident_Int (6));
   begin
      Write (File1, Int);
      Write (File1, Int);
      if Index (File1) /= Three_Pc then
         Failed ("INCORRECT INDEX VALUE - 1");
      end if;
      if Size (File1) /= Two_C then
         Failed ("INCORRECT SIZE VALUE - 2");
      end if;

      Write (File1, Int, Five_Pc);
      if Index (File1) /= Six_Pc then
         Failed ("INCORRECT INDEX VALUE - 3");
      end if;
      if Size (File1) /= Five_C then
         Failed ("INCORRECT SIZE VALUE - 4");
      end if;
   end;

   begin
      Delete (File1);
   exception
      when Use_Error =>
         null;
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce2409a;
