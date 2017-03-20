-- C39006F2.ADA

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
--     CHECK THAT NO PROGRAM_ERROR IS RAISED IF A SUBPROGRAM'S BODY HAS
--     BEEN ELABORATED BEFORE IT IS CALLED.  CHECK THE FOLLOWING:
--        B) FOR A SUBPROGRAM LIBRARY UNIT USED IN ANOTHER UNIT, NO
--           PROGRAM_ERROR IS RAISED IF PRAGMA ELABORATE NAMES THE
--           SUBPROGRAM.

--     THIS LIBRARY PACKAGE BODY IS USED BY C39006F3M.ADA.

-- HISTORY:
--     TBN  08/22/86  CREATED ORIGINAL TEST.
--     BCB  03/29/90  CORRECTED HEADER.  CHANGED TEST NAME IN CALL
--                    TO 'TEST'.
--     PWN  05/25/94  ADDED A PROCEDURE TO KEEP PACKAGE BODIES LEGAL.

with C39006f0;
with Report; use Report;
pragma Elaborate (C39006f0, Report);

package body C39006f1 is

   procedure Require_Body is
   begin
      null;
   end Require_Body;

begin
   Test
     ("C39006F",
      "CHECK THAT NO PROGRAM_ERROR IS RAISED IF A " &
      "SUBPROGRAM'S BODY HAS BEEN ELABORATED " &
      "BEFORE IT IS CALLED, WHEN A SUBPROGRAM " &
      "LIBRARY UNIT IS USED IN ANOTHER UNIT AND " &
      "PRAGMA ELABORATE IS USED");
   begin
      declare
         Var1 : Integer := C39006f0 (Ident_Int (1));
      begin
         if Var1 /= Ident_Int (1) then
            Failed ("INCORRECT RESULTS - 1");
         end if;
      end;
   exception
      when Program_Error =>
         Failed ("PROGRAM_ERROR RAISED - 1");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 1");
   end;

   declare
      Var2 : Integer := 1;

      procedure Check (B : in out Integer) is
      begin
         B := C39006f0 (Ident_Int (2));
      exception
         when Program_Error =>
            Failed ("PROGRAM_ERROR RAISED - 2");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 2");
      end Check;
   begin
      Check (Var2);
      if Var2 /= Ident_Int (2) then
         Failed ("INCORRECT RESULTS - 2");
      end if;
   end;

   declare
      package P is
         Var3 : Integer;
      end P;

      package body P is
      begin
         Var3 := C39006f0 (Ident_Int (3));
         if Var3 /= Ident_Int (3) then
            Failed ("INCORRECT RESULTS - 3");
         end if;
      exception
         when Program_Error =>
            Failed ("PROGRAM_ERROR RAISED - 3");
         when others =>
            Failed ("UNEXPECTED EXCEPTION - 3");
      end P;
   begin
      null;
   end;

   declare
      generic
         Var4 : Integer := 1;
      package Q is
         type Array_Typ1 is array (1 .. Var4) of Integer;
         Array_1 : Array_Typ1;
      end Q;

      package New_Q is new Q (C39006f0 (Ident_Int (4)));

      use New_Q;

   begin
      if Array_1'Last /= Ident_Int (4) then
         Failed ("INCORRECT RESULTS - 4");
      end if;
   end;

end C39006f1;
