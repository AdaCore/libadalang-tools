-- C39006E.ADA

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
-- CHECK THAT PROGRAM_ERROR IS NOT RAISED IF A SUBPROGRAM'S BODY HAS
-- BEEN ELABORATED BEFORE IT IS CALLED. CHECK THE FOLLOWING:
--     A) A SUBPROGRAM CAN APPEAR IN A NON-ELABORATED DECLARATIVE PART
--        OR PACKAGE SPECIFICATION BEFORE ITS BODY.

-- TBN  8/21/86

with Report; use Report;
procedure C39006e is

begin
   Test
     ("C39006E",
      "CHECK THAT PROGRAM_ERROR IS NOT RAISED IF A " &
      "SUBPROGRAM IS CALLED IN A NON-ELABORATED " &
      "DECLARATIVE PART OR PACKAGE SPECIFICATION " &
      "BEFORE ITS BODY IS ELABORATED");
   declare -- (A)

      function Init_1 (A : Integer) return Integer;

      package P is
         procedure Use_Init1;
      end P;

      package body P is
         procedure Use_Init1 is
         begin
            if not Equal (3, 3) then
               declare
                  X : Integer := Init_1 (1);
               begin
                  null;
               end;
            else
               null;
            end if;

         exception
            when Program_Error =>
               Failed ("PROGRAM_ERROR RAISED - 1");
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 1");
         end Use_Init1;

      begin
         Use_Init1;
      end P;

      function Init_1 (A : Integer) return Integer is
      begin
         return (A + Ident_Int (1));
      end Init_1;

   begin -- (A)
      null;
   end; -- (A)

   declare -- (B)

      procedure Init_2 (A : in out Integer);

      package P is
         function Use_Init2 return Boolean;
      end P;

      package body P is
         function Use_Init2 return Boolean is
         begin
            if not Equal (3, 3) then
               declare
                  X : Integer;
               begin
                  Init_2 (X);
               end;
            end if;
            return Ident_Bool (False);

         exception
            when Program_Error =>
               Failed ("PROGRAM_ERROR RAISED - 2");
               return False;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED - 2");
               return False;
         end Use_Init2;
      begin
         if Use_Init2 then
            Failed ("INCORRECT RESULTS FROM FUNCTION CALL - 2");
         end if;
      end P;

      procedure Init_2 (A : in out Integer) is
      begin
         A := A + Ident_Int (1);
      end Init_2;

   begin -- (B)
      null;
   end; -- (B)

   declare -- (C)
      function Init_3 return Integer;

      package Q is
         Var : Integer;
      end Q;

      package body Q is
      begin
         if not Equal (3, 3) then
            Var := Init_3;
         end if;
      exception
         when Program_Error =>
            Failed ("PROGRAM_ERROR RAISED - 3");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 3");
      end Q;

      function Init_3 return Integer is
      begin
         return Ident_Int (1);
      end Init_3;

   begin -- (C)
      null;
   end; -- (C)

   declare -- (D)
      procedure Init_4 (A : in out Integer);

      package Q is
         Var : Integer := 1;
      end Q;

      package body Q is
      begin
         if not Equal (3, 3) then
            Init_4 (Var);
         end if;
      exception
         when Program_Error =>
            Failed ("PROGRAM_ERROR RAISED - 4");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 4");
      end Q;

      procedure Init_4 (A : in out Integer) is
      begin
         A := Ident_Int (4);
      end Init_4;

   begin -- (D)
      null;
   end; -- (D)

   begin -- (E)

      declare
         function Init_5 (A : Integer) return Integer;

         procedure Use_Init5 is
            package Q is
               X : Integer := Init_5 (1);
            end Q;
            use Q;
         begin
            X := Ident_Int (5);

         end Use_Init5;

         function Init_5 (A : Integer) return Integer is
         begin
            return (A + Ident_Int (1));
         end Init_5;

      begin
         Use_Init5;
      end;

   exception
      when Program_Error =>
         Failed ("PROGRAM_ERROR RAISED - 5");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 5");

   end; -- (E)

   Result;
end C39006e;
