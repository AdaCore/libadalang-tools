-- CE3804F.ADA

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
--     CHECK THAT FLOAT_IO GET RAISES CONSTRAINT_ERROR WHEN THE VALUE
--     SUPPLIED BY WIDTH IS NEGATIVE, WIDTH IS GREATER THAN FIELD'LAST
--     WHEN FIELD'LAST IS LESS THAN INTEGER'LAST, OR THE VALUE READ IS
--     OUT OF RANGE OF THE ITEM PARAMETER, BUT WITHIN THE RANGE OF THE
--     SUBTYPE USED TO INSTANTIATE FLOAT_IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 09/07/82
--     JBG 08/30/83
--     DWC 09/11/87  SPLIT CASE FOR FIXED_IO INTO CE3804P.ADA AND
--                   CORRECTED EXCEPTION HANDLING.
--     JRL 06/07/96  Added call to Ident_Int in expressions involving
--                   Field'Last, to make the expressions non-static and
--                   prevent compile-time rejection.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3804f is
   Incomplete : exception;

begin

   Test
     ("CE3804F",
      "CHECK THAT FLOAT_IO GET RAISES " &
      "CONSTRAINT_ERROR WHEN THE VALUE SUPPLIED " &
      "BY WIDTH IS NEGATIVE, WIDTH IS GREATER THAN " &
      "FIELD'LAST WHEN FIELD'LAST IS LESS THAN " &
      "INTEGER'LAST, OR THE VALUE READ IS OUT OF " &
      "RANGE OF THE ITEM PARAMETER, BUT WITHIN THE " &
      "RANGE OF THE SUBTYPE USED TO INSTANTIATE " & "FLOAT_IO.");

   declare
      Ft : File_Type;
      type Flt is new Float range 1.0 .. 10.0;
      package Fl_Io is new Float_Io (Flt);
      use Fl_Io;
      X : Flt range 1.0 .. 5.0;

   begin
      begin
         Get (Ft, X, Ident_Int (-3));
         Failed ("CONSTRAINT_ERROR NOT RAISED FOR NEGATIVE " & "WIDTH");
      exception
         when Constraint_Error =>
            null;
         when Status_Error =>
            Failed
              ("STATUS_ERROR RAISED INSTEAD OF " &
               "CONSTRAINT_ERROR FOR NEGATIVE WIDTH");
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR NEGATIVE " & "WIDTH");
      end;

      if Field'Last < Integer'Last then
         begin
            Get (X, Field'Last + Ident_Int (1));
            Failed
              ("CONSTRAINT_ERROR NOT RAISED - " &
               "FIELD'LAST + 1 WIDTH - DEFAULT");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED - " &
                  "FIELD'LAST + 1 WIDTH - DEFAULT");
         end;
      end if;

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

      Put (Ft, "1.0");
      New_Line (Ft);
      Put (Ft, "8.0");
      New_Line (Ft);
      Put (Ft, "2.0");
      New_Line (Ft);
      Put (Ft, "3.0");

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT OPEN " & "FOR IN_FILE MODE");
            raise Incomplete;
      end;

      Get (Ft, X);
      if X /= 1.0 then
         Failed ("WRONG VALUE READ WITH EXTERNAL FILE");
      end if;

      begin
         Get (Ft, X);
         Failed
           ("CONSTRAINT_ERROR NOT RAISED - " &
            "VALUE OUT OF RANGE WITH EXTERNAL FILE");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED - " &
               "VALUE OUT OF RANGE WITH EXTERNAL FILE");
      end;

      begin
         Get (Ft, X, Ident_Int (-1));
         Failed
           ("CONSTRAINT_ERROR NOT RAISED - " &
            "NEGATIVE WIDTH WITH EXTERNAL FILE");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED - " &
               "NEGATIVE WIDTH WITH EXTERNAL FILE");
      end;

      Skip_Line (Ft);

      if Field'Last < Integer'Last then
         begin
            Get (Ft, X, Field'Last + Ident_Int (1));
            Failed
              ("CONSTRAINT_ERROR NOT RAISED - " &
               "FIELD'LAST + 1 WIDTH WITH " & "EXTERNAL FILE");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED - " & "FIELD'LAST + 1 WIDTH WITH " &
                  "EXTERNAL FILE");
         end;
      end if;

      Skip_Line (Ft);

      begin
         Get (Ft, X, 3);
      exception
         when Constraint_Error =>
            Failed
              ("CONSTRAINT_ERROR RAISED - " &
               "OUT OF RANGE WITH EXTERNAL FILE");
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED - " &
               "OUT OF RANGE WITH EXTERNAL FILE");
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
end Ce3804f;