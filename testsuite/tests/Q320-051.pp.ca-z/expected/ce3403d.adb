-- CE3403D.ADA

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
--     CHECK THAT SKIP_LINE RAISES CONSTRAINT_ERROR IF SPACING IS
--     ZERO OR NEGATIVE.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/16/82
--     SPS 11/11/82
--     DWC 09/09/87  ADDED CASE FOR COUNT'LAST.
--     KAS 11/27/95  REMOVED CASES FOR COUNT'LAST
--     TMB 11/19/96  FIXED OBJECTIVE

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3403d is

   File : File_Type;

begin

   Test
     ("CE3403D",
      "CHECK THAT SKIP_LINE RAISES " &
      "CONSTRAINT_ERROR IF SPACING IS ZERO, " & "OR NEGATIVE");
   begin
      Skip_Line (File, Positive_Count (Ident_Int (0)));
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR ZERO");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR ZERO");
   end;

   begin
      Skip_Line (File, Positive_Count (Ident_Int (-2)));
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR NEGATIVE NUMBER");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR " & "NEGATIVE NUMBER");
   end;

   begin
      Skip_Line (Positive_Count (Ident_Int (0)));
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR ZERO - DEFAULT");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED FOR ZERO " & "- DEFAULT");
   end;

   begin
      Skip_Line (Positive_Count (Ident_Int (-6)));
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR NEGATIVE NUM " & "- DEFAULT");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED NEGATIVE NUM " & "- DEFAULT");
   end;

   Result;

end Ce3403d;
