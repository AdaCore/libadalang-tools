-- CE2201F.ADA

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
--      CHECK THAT READ, WRITE, AND END_OF_FILE ARE SUPPORTED FOR
--      SEQUENTIAL FILES WITH PRIVATE ELEMENT_TYPES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES WITH PRIVATE ELEMENT_TYPES.

-- HISTORY:
--     ABW 08/17/82
--     SPS 09/15/82
--     SPS 11/09/82
--     JBG 01/06/83
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/03/87  REMOVED DEPENDENCE OF RESET AND CREATED EXTERNAL
--                   FILES RATHER THAN TEMPORARY FILES.

with Report; use Report;
with Sequential_Io;

procedure Ce2201f is

   package Pkg is
      type Priv is private;
      function Make_Priv (X : Integer) return Priv;
   private
      type Priv is new Integer;
   end Pkg;
   use Pkg;

   package body Pkg is
      function Make_Priv (X : Integer) return Priv is
      begin
         return Priv (X);
      end Make_Priv;
   end Pkg;

begin

   Test
     ("CE2201F",
      "CHECK THAT READ, WRITE, AND " &
      "END_OF_FILE ARE SUPPORTED FOR " &
      "SEQUENTIAL FILES FOR PRIVATE TYPES");

   declare
      package Seq_Prv is new Sequential_Io (Priv);
      use Seq_Prv;
      Prv, Item_Prv : Priv;
      File_Prv      : File_Type;
      Incomplete : exception;
   begin
      begin
         Create (File_Prv, Out_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("CREATE OF SEQUENTIAL FILE WITH " &
               "MODE OUT_FILE NOT SUPPORTED");
            raise Incomplete;
      end;

      Prv := Make_Priv (Ident_Int (26));

      Write (File_Prv, Prv);
      Close (File_Prv);

      begin
         Open (File_Prv, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("OPEN OF SEQUENTIAL FILE WITH " & "MODE IN_FILE NOT SUPPORTED");
            raise Incomplete;
      end;

      if End_Of_File (File_Prv) then
         Failed ("WRONG END_OF_FILE VALUE FOR PRIVATE TYPE");
      end if;

      Read (File_Prv, Item_Prv);

      if Item_Prv /= Make_Priv (26) then
         Failed ("READ WRONG VALUE");
      end if;

      if not End_Of_File (File_Prv) then
         Failed ("NOT AT END OF FILE");
      end if;

      begin
         Delete (File_Prv);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce2201f;
