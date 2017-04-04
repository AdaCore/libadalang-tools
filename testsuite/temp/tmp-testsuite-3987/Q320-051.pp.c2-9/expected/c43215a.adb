-- C43215A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A POSITIONAL
-- ARRAY AGGREGATE WHOSE UPPER BOUND EXCEEDS THE UPPER BOUND
-- OF THE INDEX SUBTYPE BUT BELONGS TO THE INDEX BASE TYPE.

-- EG  02/13/84

with Report;
with System;

procedure C43215a is

   use Report;
   use System;

begin

   Test
     ("C43215A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
      "FOR A POSITIONAL ARRAY AGGREGATE WHOSE " &
      "UPPER BOUND EXCEEDS THE UPPER BOUND OF THE " &
      "INDEX SUBTYPE BUT BELONGS TO THE INDEX " &
      "BASE TYPE");

   begin

      Case_A : declare

         Lower_Bound : constant := Max_Int - 3;
         Upper_Bound : constant := Max_Int - 1;

         type Sta is range Lower_Bound .. Upper_Bound;

         type Ta is array (Sta range <>) of Integer;

         A1 : Ta (Sta);
         Ok : exception;

         function Fun1 return Ta is
         begin
            return (1, 2, 3, 4);
         exception
            when Constraint_Error =>
               begin
                  Comment ("CASE A : CONSTRAINT_ERROR RAISED");
                  raise Ok;
               end;
            when others =>
               begin
                  Failed ("CASE A : EXCEPTION RAISED IN FUN1");
                  raise Ok;
               end;
         end Fun1;

      begin

         A1 := Fun1;
         Failed ("CASE A : CONSTRAINT_ERROR NOT RAISED");

      exception

         when Ok =>
            null;

         when others =>
            Failed ("CASE A : EXCEPTION RAISED");

      end Case_A;

      Case_B : declare

         type Enum is (A, B, C, D);

         subtype Stb is Enum range A .. C;

         type Tb is array (Stb range <>) of Integer;

         B1 : Tb (Stb);
         Ok : exception;

         function Fun1 return Tb is
         begin
            return (1, 2, 3, 4);
         exception
            when Constraint_Error =>
               begin
                  Comment ("CASE B : CONSTRAINT_ERROR RAISED");
                  raise Ok;
               end;
            when others =>
               begin
                  Failed ("CASE B : EXCEPTION RAISED IN FUN1");
                  raise Ok;
               end;
         end Fun1;

      begin

         B1 := Fun1;
         Failed ("CASE B : CONSTRAINT_ERROR NOT RAISED");

      exception

         when Ok =>
            null;

         when others =>
            Failed ("CASE B : EXCEPTION RAISED");

      end Case_B;

   end;

   Result;

end C43215a;
