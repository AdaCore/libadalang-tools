-- CE2201G.ADA

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
--      CHECK THAT READ, WRITE, AND END_OF_FILE ARE SUPPORTED
--      FOR SEQUENTIAL FILES WITH VARIANT RECORDS WITH DEFAULT
--      DISCRIMINANTS.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     TBN 05/15/86
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/03/87  REMOVED DEPENDENCE OF RESET AND CREATED EXTERNAL
--                   FILES RATHER THAN TEMPORARY FILES.

with Report; use Report;
with Sequential_Io;

procedure Ce2201g is

begin

   Test
     ("CE2201G",
      "CHECK THAT READ, WRITE, AND END_OF_FILE " &
      "ARE SUPPORTED FOR SEQUENTIAL FILES WITH " &
      "UNCONSTRAINED VARIANT RECORD TYPES WITH " & "DEFAULT DISCRIMINANTS.");

   declare
      type Var_Rec (Discr : Boolean := True) is record
         case Discr is
            when True =>
               A : Integer;
            when False =>
               B : String (1 .. 20);
         end case;
      end record;

      package Seq_Var_Rec is new Sequential_Io (Var_Rec);
      use Seq_Var_Rec;

      File_Var_Rec : File_Type;
      Incomplete : exception;
      Item_True : Var_Rec (True);     -- CONSTRAINED
      Item      : Var_Rec;           -- UNCONSTRAINED

   begin
      begin
         Create (File_Var_Rec, Out_File, Legal_File_Name);
      exception
         when Use_Error | Name_Error =>
            Not_Applicable
              ("CREATE OF SEQUENTIAL FILE WITH " &
               "MODE OUT_FILE NOT SUPPORTED");
            raise Incomplete;
      end;

      Write (File_Var_Rec, (True, -5));
      Write (File_Var_Rec, (False, (1 .. 20 => 'B')));
      Close (File_Var_Rec);

      begin
         Open (File_Var_Rec, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("OPEN OF SEQUENTIAL FILE WITH " & "MODE IN_FILE NOT SUPPORTED");
            raise Incomplete;
      end;

      if End_Of_File (File_Var_Rec) then
         Failed ("WRONG END_OF_FILE VALUE FOR RECORD" & "WITH DISCRIMINANT");
      end if;

      begin
         Read (File_Var_Rec, Item_True);

         if Item_True /= (True, Ident_Int (-5)) then
            Failed ("READ WRONG VALUE - 1");
         end if;

         if End_Of_File (File_Var_Rec) then
            Failed ("PREMATURE END OF FILE");
         end if;

         Read (File_Var_Rec, Item);

         if Item /= (False, (1 .. Ident_Int (20) => 'B')) then
            Failed ("READ WRONG VALUE - 2");
         end if;

         if not End_Of_File (File_Var_Rec) then
            Failed ("NOT AT END OF FILE");
         end if;

      end;

      begin
         Delete (File_Var_Rec);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce2201g;
