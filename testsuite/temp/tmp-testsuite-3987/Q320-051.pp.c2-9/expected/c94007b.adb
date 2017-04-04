-- C94007B.ADA

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
-- CHECK THAT A TASK THAT IS ALLOCATED IN A NON-LIBRARY PACKAGE
--   (SPECIFICATION OR BODY) DOES NOT "DEPEND" ON THE PACKAGE,
--   BUT ON THE INNERMOST ENCLOSING BLOCK, SUBPROGRAM BODY,
--   OR TASK BODY.
-- SUBTESTS ARE:
--   (A)  A SIMPLE TASK ALLOCATOR, IN A VISIBLE PART, IN A BLOCK.
--   (B)  A RECORD OF TASK ALLOCATOR, IN A PRIVATE PART, IN A FUNCTION.
--   (C)  A RECORD OF ARRAY OF TASK ALLOCATOR, IN A PACKAGE BODY,
--           IN A TASK BODY.

-- JRK 10/16/81
-- SPS 11/2/82
-- PWN 01/31/95  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C94007b is

   task type Sync is
      entry Id (C : Character);
      entry Inner;
      entry Outer;
   end Sync;

   task body Sync is
      Id_C : Character;
   begin
      accept Id (C : Character) do
         Id_C := C;
      end Id;
      delay 1.0;
      select
         accept Outer;
      or
         delay 120.0;
         Failed ("PROBABLY BLOCKED - (" & Id_C & ')');
      end select;
      accept Inner;
   end Sync;

begin
   Test
     ("C94007B",
      "CHECK THAT A TASK THAT IS ALLOCATED IN A " &
      "NON-LIBRARY PACKAGE (SPECIFICATION OR BODY) " &
      "DOES NOT ""DEPEND"" ON THE PACKAGE, BUT ON " &
      "THE INNERMOST ENCLOSING BLOCK, SUBPROGRAM " &
      "BODY, OR TASK BODY");

   --------------------------------------------------

   declare -- (A)

      S : Sync;

   begin -- (A)

      S.Id ('A');

      declare

         package Pkg is
            task type Tt is
               entry E;
            end Tt;
            type A_T is access Tt;
            A : A_T;
         end Pkg;

         package body Pkg is
            task body Tt is
            begin
               S.Inner;  -- PROBABLE INNER BLOCK POINT.
            end Tt;
         begin
            A := new Tt;
         end Pkg;            -- PROBABLE OUTER BLOCK POINT.

      begin

         S.Outer;

      exception
         when Tasking_Error =>
            null;
      end;

   end; -- (A)

   --------------------------------------------------

   declare -- (B)

      S : Sync;

      I : Integer;

      function F return Integer is

         package Pkg is
         private
            task type Tt is
               entry E;
            end Tt;

            type Rt is record
               T : Tt;
            end record;

            type Art is access Rt;

            Ar : Art;
         end Pkg;

         package body Pkg is
            task body Tt is
            begin
               S.Inner;  -- PROBABLE INNER BLOCK POINT.
            end Tt;
         begin
            Ar := new Rt;
         end Pkg;            -- PROBABLE OUTER BLOCK POINT.

      begin -- F

         S.Outer;
         return 0;

      exception
         when Tasking_Error =>
            return 0;
      end F;

   begin -- (B)

      S.Id ('B');
      I := F;

   end; -- (B)

   --------------------------------------------------

   declare -- (C)

      S : Sync;

   begin -- (C)

      S.Id ('C');

      declare

         task Tsk is
         end Tsk;

         task body Tsk is

            package Pkg is
            end Pkg;

            package body Pkg is
               task type Tt is
                  entry E;
               end Tt;

               type Arr is array (1 .. 1) of Tt;
               type Rat is record
                  T : Arr;
               end record;

               type Arat is access Rat;

               Ara : Arat;

               task body Tt is
               begin
                  S.Inner;  -- PROBABLE INNER BLOCK POINT.
               end Tt;
            begin
               Ara := new Rat;
            end Pkg;            -- PROBABLE OUTER BLOCK POINT.

         begin -- TSK

            S.Outer;

         exception
            when Tasking_Error =>
               null;
         end Tsk;

      begin
         null;
      end;

   end; -- (C)

   --------------------------------------------------

   Result;
end C94007b;
