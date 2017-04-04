-- C39006B.ADA

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
-- CHECK THAT PROGRAM_ERROR IS RAISED IF AN ATTEMPT IS MADE TO CALL A
-- SUBPROGRAM WHOSE BODY HAS NOT YET BEEN ELABORATED. CHECK THE FOLLOWING:
--     B) THE SUBPROGRAM IS CALLED IN A PACKAGE BODY.
--     C) THE SUBPROGRAM IS AN ACTUAL GENERIC PARAMETER CALLED DURING
--        ELABORATION OF THE GENERIC INSTANTIATION.
--     D) THE SUBPROGRAM IS CALLED DURING ELABORATION OF AN OPTIONAL
--        PACKAGE BODY.

-- TBN  8/19/86

with Report; use Report;
procedure C39006b is

begin
   Test
     ("C39006B",
      "CHECK THAT PROGRAM_ERROR IS RAISED IF AN " &
      "ATTEMPT IS MADE TO CALL A SUBPROGRAM WHOSE " &
      "BODY HAS NOT YET BEEN ELABORATED");
   begin
      declare
         package Pack is
            function Fun return Integer;
            procedure Proc (A : in out Integer);
         end Pack;

         package body Pack is

            Var1 : Integer := 0;

            procedure Proc (A : in out Integer) is
            begin
               if A = Ident_Int (1) then
                  A := A + Fun;
                  Failed ("PROGRAM_ERROR NOT RAISED - 1");
               else
                  A := Ident_Int (1);
               end if;
            exception
               when Program_Error =>
                  null;
               when others =>
                  Failed ("UNEXPECTED EXCEPTION RAISED " & "1");
            end Proc;

            package Inside is
            end Inside;

            package body Inside is
            begin
               Proc (Var1);
               Proc (Var1);
            end Inside;

            function Fun return Integer is
            begin
               return (Ident_Int (1));
            end Fun;

         begin
            null;
         end Pack;

      begin
         null;
      end;
   end;

   begin
      declare
         function Init_2 return Integer;

         generic
            with function Ff return Integer;
         package P is
            Y : Integer;
         end P;

         Global_Int : Integer := Ident_Int (1);

         package body P is
         begin
            if Global_Int = 1 then
               Y := Ff;
            end if;
         end P;

         package N is
            package New_P is new P (Init_2);
         end N;

         function Init_2 return Integer is
         begin
            return (Ident_Int (1));
         end Init_2;

      begin
         Failed ("PROGRAM_ERROR NOT RAISED - 2");
      end;

   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 2");
   end;

   declare

      procedure Add1 (A : in out Integer);

      package P is
         Var : Integer := Ident_Int (1);
      end P;

      package body P is
      begin
         if Var = 1 then
            Add1 (Var);
            Failed ("PROGRAM_ERROR NOT RAISED - 3");
         end if;
      exception
         when Program_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 3");
      end P;

      procedure Add1 (A : in out Integer) is
      begin
         A := A + Ident_Int (1);
      end Add1;

   begin
      null;
   end;

   Result;
end C39006b;
