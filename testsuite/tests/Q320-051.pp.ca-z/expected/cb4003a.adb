-- CB4003A.ADA

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
--     CHECK THAT EXCEPTIONS RAISED DURING ELABORATION OF PACKAGE
--     SPECIFICATIONS, OR DECLARATIVE_PARTS OF BLOCKS AND PACKAGE
--     BODIES, ARE PROPAGATED TO THE STATIC ENVIRONMENT.  EXCEPTIONS
--     ARE CAUSED BY INITIALIZATIONS AND FUNCTION CALLS.

-- HISTORY:
--     DAT 04/14/81  CREATED ORIGINAL TEST.
--     JET 01/06/88  UPDATED HEADER FORMAT AND ADDED CODE TO
--                   PREVENT OPTIMIZATION.

with Report; use Report;

procedure Cb4003a is

   E : exception;

   function F (B : Boolean) return Integer is
   begin
      if B then
         raise E;
      else
         return 1;
      end if;
   end F;

begin
   Test
     ("CB4003A",
      "CHECK THAT EXCEPTIONS DURING ELABORATION" & " OF DECLARATIVE PARTS" &
      " IN BLOCKS, PACKAGE SPECS, AND PACKAGE BODIES ARE" &
      " PROPAGATED TO STATIC ENCLOSING ENVIRONMENT");

   begin
      declare
         package P1 is
            I : Integer range 1 .. 1 := 2;
         end P1;
      begin
         Failed ("EXCEPTION NOT RAISED 1");
         if not Equal (P1.I, P1.I) then
            Comment ("NO EXCEPTION RAISED");
         end if;
      exception
         when others =>
            Failed ("WRONG HANDLER 1");
      end;
      Failed ("EXCEPTION NOT RAISED 1A");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION 1");
   end;

   for L in Ident_Int (1) .. Ident_Int (4) loop
      begin
         declare
            package P2 is
            private
               J : Integer range 2 .. 4 := L;
            end P2;

            Q : Integer := F (L = 3);

            package body P2 is
               K : Integer := F (L = 2);

            begin
               if not (Equal (J, J) or Equal (K, K)) then
                  Comment ("CAN'T OPTIMIZE THIS");
               end if;
            end P2;
         begin
            if L /= 4 then
               Failed ("EXCEPTION NOT RAISED 2");
            end if;

            if not Equal (Q, Q) then
               Comment ("CAN'T OPTIMIZE THIS");
            end if;

            exit;
         exception
            when others =>
               Failed ("WRONG EXCEPTION HANDLER 2");
               exit;
         end;
         Failed ("EXCEPTION NOT RAISED 2A");
      exception
         when E | Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED 2");
      end;
   end loop;

   Result;

end Cb4003a;
