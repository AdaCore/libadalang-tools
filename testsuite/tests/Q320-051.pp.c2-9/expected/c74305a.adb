-- C74305A.ADA

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
--    CHECK THAT A DEFERRED CONSTANT CAN BE USED AS A DEFAULT
--    INITIALIZATION FOR A PARAMETER OR AS A DEFAULT INITIA-
--    LIZATION FOR A COMPONENT (NON GENERIC CASE).

-- DAT  4/06/81
-- RM   5/21/81
-- SPS  8/23/82
-- SPS  2/10/83
-- SPS 10/20/83
-- EG  12/20/83
-- GJD 11/15/95 REMOVED ADA 95 INCOMPATIBILITY.

with Report;

procedure C74305a is

   use Report;

   package Pk is
      type T1 is private;
      type T2 is private;
      C1 : constant T1;                   -- OK.

      procedure P1 (P : T1 := C1);        -- OK.

      type R1 is record
         C : T1 := C1;                  -- OK.
      end record;
   private
      procedure Proc2 (P : T1 := C1);     -- OK.

      type R2 is record
         C : T1      := C1;                  -- OK.
         D : Integer := C1'Size;        -- OK.
      end record;

      function F1 (P : T1) return T1;

      type T1 is new Integer;
      type T2 is array (1 .. 2) of Integer; -- OK.

      function F2 (P : T1) return T1;

      procedure P3 (P : T1 := C1 + 1);      -- OK.

      procedure P4 (P : T1 := F1 (C1));

      type R5 is record
         C : T1 := F2 (C1);
      end record;

      procedure P5 (P : T1 := C1 + 2) renames P3;

      type R3 is record
         C : T1 := C1;                  -- OK.
      end record;

      C1 : constant T1 := 1;              -- OK.
      C2 : constant T2 := (1, 1);          -- OK.
   end Pk;

   use Pk;

   package body Pk is

      R11 : R1;

      procedure P1 (P : T1 := C1) is
      begin
         if (P /= 1) then
            Failed ("PARAMETER DEFAULT OF P1 NOT PROPERLY " & "INITIALIZED");
         end if;
      end P1;

      procedure Proc2 (P : T1 := C1) is
      begin
         null;
      end Proc2;

      procedure P3 (P : T1 := C1 + 1) is
      begin
         if (P /= 3) then
            Failed ("PARAMETER DEFAULT OF P5 NOT PROPERLY " & "INITIALIZED");
         end if;
      end P3;

      function F1 (P : T1) return T1 is
      begin
         return P + 10;
      end F1;

      procedure P4 (P : T1 := F1 (C1)) is
      begin
         if (P /= 11) then
            Failed ("WRONG ACTUAL PARAMETER RECEIVED");
         end if;
      end P4;

      function F2 (P : T1) return T1 is
      begin
         return P + 20;
      end F2;

   begin -- PK BODY.

      declare

         R55 : R5;

      begin
         Test
           ("C74305A",
            "CHECK THAT A DEFERRED CONSTANT CAN " &
            "BE USED AS A DEFAULT INITIALIZATION " &
            "FOR A PARAMETER OR AS A DEFAULT " &
            "INITIALIZATION FOR A COMPONENT (NON " & "GENERIC CASE)");

         if (R11.C /= 1) then
            Failed ("RECORD R11 NOT PROPERLY INITIALIZED");
         end if;

         P4;

         if (R55.C /= 21) then
            Failed ("RECORD R55 NOT PROPERLY INITIALIZED");
         end if;

         P5;
      end;
   end Pk;

begin

   P1;

   Result;
end C74305a;
