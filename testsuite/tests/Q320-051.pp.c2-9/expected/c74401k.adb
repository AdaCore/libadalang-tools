-- C74401K.ADA

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
-- CHECK THAT OUT PARAMETERS OF AN ENTRY DECLARATION CAN HAVE A LIMITED PRIVATE
-- TYPE IF THE ENTRY DECLARATION OCCURS IN THE VISIBLE PART OF A PACKAGE
-- SPECIFICATION, INCLUDING WITHIN PACKAGES NESTED IN A VISIBLE PART.

-- CHECK THAT A RENAMING DECLARATION CAN RENAME AN ENTRY DECLARED WITH AN OUT
-- PARAMETER.

-- JBG 5/1/85

with Report; use Report;
procedure C74401k is

   package Pkg is
      type Lp is limited private;
      task P20 is
         entry Tp20 (X : out Lp);        -- OK.
         entry Reset (X : out Lp);
      end P20;
      function Eq (L, R : Lp) return Boolean;
      Val1 : constant Lp;

      package Nested is
         task Nest1 is
            entry Tnest1 (X : out Lp);
         end Nest1;
      private
         task Nest2 is
            entry Tnest2 (X : out Lp);
         end Nest2;
      end Nested;
   private
      type Lp is new Integer;
      Val1 : constant Lp := Lp (Ident_Int (3));
   end Pkg;

   Var : Pkg.Lp;

   package body Pkg is
      task body P20 is
      begin
         loop
            select
               accept Tp20 (X : out Lp) do
                  X := 3;
               end Tp20;
            or
               accept Reset (X : out Lp) do
                  X := 0;
               end Reset;
            or
               terminate;
            end select;
         end loop;
      end P20;

      function Eq (L, R : Lp) return Boolean is
      begin
         return L = R;
      end Eq;

      package body Nested is
         task body Nest1 is
         begin
            accept Tnest1 (X : out Lp) do
               X := 3;
            end Tnest1;
         end Nest1;

         task body Nest2 is
         begin
            null;
         end Nest2;
      end Nested;
   begin
      Var := Lp (Ident_Int (0));
   end Pkg;

   package Pkg1 is
      procedure P21 (X : out Pkg.Lp) renames Pkg.P20.Tp20;   -- OK:
      -- RENAMING.
   end Pkg1;

begin

   Test
     ("C74401K",
      "CHECK THAT A PROCEDURE CAN HAVE AN OUT " &
      "PARAMETER WITH A LIMITED PRIVATE TYPE");

   Pkg.P20.Reset (Var);
   Pkg.P20.Tp20 (Var);

   if not Pkg.Eq (Var, Pkg.Val1) then
      Failed ("DIRECT CALL NOT CORRECT");
   end if;

   Pkg.P20.Reset (Var);
   Pkg1.P21 (Var);

   if not Pkg.Eq (Var, Pkg.Val1) then
      Failed ("RENAMED CALL NOT CORRECT");
   end if;

   Pkg.P20.Reset (Var);
   Pkg.Nested.Nest1.Tnest1 (Var);

   if not Pkg.Eq (Var, Pkg.Val1) then
      Failed ("NESTED CALL NOT CORRECT");
   end if;

   Result;

end C74401k;
