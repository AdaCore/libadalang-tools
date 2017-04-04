-- C83025C.ADA

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
--     CHECK THAT A DECLARATION IN A DECLARATIVE REGION OF A GENERIC
--     SUBPROGRAM HIDES AN OUTER DECLARATION OF A HOMOGRAPH. ALSO CHECK
--     THAT THE OUTER DECLARATION IS DIRECTLY VISIBLE IN BOTH
--     DECLARATIVE REGIONS BEFORE THE DECLARATION OF THE INNER HOMOGRAPH
--     AND THE OUTER DECLARATION IS VISIBLE BY SELECTION AFTER THE INNER
--     HOMOGRAPH DECLARATION, IF THE GENERIC SUBPROGRAM BODY IS COMPILED
--     AS A SUBUNIT IN THE SAME COMPILATION.

-- HISTORY:
--     BCB 09/01/88  CREATED ORIGINAL TEST.

with Report; use Report;
pragma Elaborate (Report);
package C83025c_Pack is
   Y : Integer := Ident_Int (5);
   Z : Integer := Y;

   generic
      type T is private;
      X : T;
   function Gen_Fun return T;

   A : Integer := Ident_Int (2);
   B : Integer := A;

   Obj : Integer := Ident_Int (3);

   Flo : Float := 5.0;

   type Enum is (One, Two, Three, Four);

   Eobj : Enum := One;

   generic
      Y : Float := 2.0;
   procedure Inner (X : in out Integer);

   generic
      Y : Boolean := True;
   procedure Inner2 (X : in Integer := A; A : in out Integer);

   generic
      Y : Enum := One;
   function Inner3 (X : Integer; Z : Enum := Y) return Integer;

   generic
      Y : Enum;
   function Inner4 (X : Integer; Z : Enum := Y) return Integer;

   generic
      Y : Character := 'A';
   procedure Inner5 (X : in out Integer; F : in Float; Z : Character := Y);
end C83025c_Pack;
