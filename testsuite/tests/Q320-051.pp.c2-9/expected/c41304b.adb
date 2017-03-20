-- C41304B.ADA

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
--      CHECK THAT L.R RAISES CONSTRAINT_ERROR WHEN:
--        L DENOTES A RECORD OBJECT SUCH THAT, FOR THE EXISTING
--           DISCRIMINANT VALUES, THE COMPONENT DENOTED BY R DOES
--           NOT EXIST.
--        L IS A FUNCTION CALL DELIVERING A RECORD VALUE SUCH THAT,
--           FOR THE EXISTING DISCRIMINANT VALUES, THE COMPONENT
--           DENOTED BY R DOES NOT EXIST.
--        L IS AN ACCESS OBJECT AND THE OBJECT DESIGNATED BY THE ACCESS
--           VALUE IS SUCH THAT COMPONENT R DOES NOT EXIST FOR THE
--           OBJECT'S CURRENT DISCRIMINANT VALUES.
--        L IS A FUNCTION CALL RETURNING AN ACCESS VALUE AND THE OBJECT
--           DESIGNATED BY THE ACCESS VALUE IS SUCH THAT COMPONENT R
--           DOES NOT EXIST FOR THE OBJECT'S CURRENT DISCRIMINANT
--           VALUES.

-- HISTORY:
--     TBN 05/23/86  CREATED ORIGINAL TEST.
--     JET 01/08/88  MODIFIED HEADER FORMAT AND ADDED CODE TO
--                   PREVENT OPTIMIZATION.

with Report; use Report;
procedure C41304b is

   type V (Disc : Integer := 0) is record
      case Disc is
         when 1 =>
            X : Integer;
         when others =>
            Y : Integer;
      end case;
   end record;

   type T is access V;

begin
   Test
     ("C41304B",
      "CHECK THAT L.R RAISES CONSTRAINT_ERROR WHEN " &
      "THE COMPONENT DENOTED BY R DOES NOT EXIST");

   declare

      Vr : V := (Disc => 0, Y => 4);
      J  : Integer;

   begin

      if Equal (4, 4) then
         Vr := (Disc => 1, X => 3);
      end if;

      J := Vr.Y;
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR A RECORD OBJECT");

      -- IF STATEMENT PREVENTS OPTIMIZING OF VARIABLE J.

      if Equal (J, 3) then
         Failed ("CONSTRAINT_ERROR NOT RAISED - 1");
      end if;

   exception

      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR A RECORD OBJECT");

   end;

   --------------------------------------------------

   declare

      J : Integer;

      function F return V is
      begin
         if Equal (4, 4) then
            return (Disc => 2, Y => 3);
         end if;
         return (Disc => 1, X => 4);
      end F;

   begin

      J := F.X;
      Failed
        ("CONSTRAINT_ERROR NOT RAISED FOR A FUNCTION CALL " &
         "DELIVERING A RECORD VALUE");

      -- IF STATEMENT PREVENTS OPTIMIZING OF VARIABLE J.

      if Equal (J, 3) then
         Failed ("CONSTRAINT_ERROR NOT RAISED - 2");
      end if;

   exception

      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR A FUNCTION CALL " &
            "DELIVERING A RECORD VALUE");

   end;

   --------------------------------------------------

   declare

      A : T := new V'(Disc => 0, Y => 4);
      J : Integer;

   begin

      if Equal (4, 4) then
         A := new V'(Disc => 1, X => 3);
      end if;

      J := A.Y;
      Failed ("CONSTRAINT_ERROR NOT RAISED FOR AN ACCESS OBJECT");

      -- IF STATEMENT PREVENTS OPTIMIZING OF VARIABLE J.

      if Equal (J, 3) then
         Failed ("CONSTRAINT_ERROR NOT RAISED - 3");
      end if;

   exception

      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR AN ACCESS OBJECT");

   end;

   --------------------------------------------------

   declare

      J : Integer;

      function F return T is
      begin
         if Equal (4, 4) then
            return new V'(Disc => 2, Y => 3);
         end if;
         return new V'(Disc => 1, X => 4);
      end F;

   begin

      J := F.X;
      Failed
        ("CONSTRAINT_ERROR NOT RAISED FOR A FUNCTION CALL " &
         "DELIVERING AN ACCESS VALUE");

      -- IF STATEMENT PREVENTS OPTIMIZING OF VARIABLE J.

      if Equal (J, 3) then
         Failed ("CONSTRAINT_ERROR NOT RAISED - 4");
      end if;

   exception

      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR A FUNCTION CALL " &
            "DELIVERING AN ACCESS VALUE");

   end;

   Result;
end C41304b;
