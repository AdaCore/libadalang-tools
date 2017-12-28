-- C83027C.ADA

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
--     CHECK THAT A DECLARATION WITHIN THE DISCRIMINANT PART OF A
--     PRIVATE TYPE DECLARATION, AN INCOMPLETE TYPE DECLARATION, AND A
--     GENERIC FORMAL TYPE DECLARATION HIDES AN OUTER DECLARATION OF A
--     HOMOGRAPH.  ALSO, CHECK THAT THE OUTER DECLARATION IS DIRECTLY
--     VISIBLE IN BOTH DECLARATIVE REGIONS BEFORE THE DECLARATION OF THE
--     INNER HOMOGRAPH AND THE OUTER DECLARATION IS VISIBLE BY SELECTION
--     AFTER THE INNER HOMOGRAPH DECLARATION.

-- HISTORY:
--     BCB 09/06/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C83027c is

   generic
      type T is private;
      X : T;
   function Gen_Fun return T;

   function Gen_Fun return T is
   begin
      return X;
   end Gen_Fun;

begin
   Test
     ("C83027C",
      "CHECK THAT A DECLARATION IN THE DISCRIMINANT " &
      "PART OF A PRIVATE TYPE DECLARATION, AN " &
      "INCOMPLETE TYPE DECLARATION, AND A GENERIC " &
      "FORMAL TYPE DECLARATION HIDES AN OUTER " &
      "DECLARATION OF A HOMOGRAPH");

   One :
   declare
      A : Integer := Ident_Int (2);

      D : Integer := Ident_Int (2);

      G : Integer := Ident_Int (2);
      H : Integer := G;

      type Rec (Z : Integer) is record
         null;
      end record;

      generic
         type Inner3 (G : Integer) is private;
      package P_One is
         type Inner
           (X : Integer := A;
            A : Integer := Ident_Int (3);
            C : Integer := One.A)
         is
           private;
         type Inner2
           (Y : Integer := D;
            D : Integer := Ident_Int (3);
            F : Integer := One.D)
         ;
         type Inner2
           (Y : Integer := D;
            D : Integer := Ident_Int (3);
            F : Integer := One.D)
         is record
            E : Integer := D;
         end record;
      private
         type Inner
           (X : Integer := A;
            A : Integer := Ident_Int (3);
            C : Integer := One.A)
         is record
            B : Integer := A;
         end record;
      end P_One;

      package body P_One is
         Recvar  : Inner;
         Recvar2 : Inner2;
         Recvar3 : Inner3 (3);
      begin
         if Recvar.A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 1");
         end if;

         if A /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 2");
         end if;

         if Recvar.B /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 3");
         end if;

         if Recvar.C /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 4");
         end if;

         if Recvar.X /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 5");
         end if;

         if Recvar2.D /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 6");
         end if;

         if D /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 7");
         end if;

         if Recvar2.E /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 8");
         end if;

         if Recvar2.F /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 9");
         end if;

         if Recvar2.Y /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 10");
         end if;

         if Recvar3.G /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 11");
         end if;

         if G /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 12");
         end if;

         if H /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 13");
         end if;
      end P_One;

      package New_P_One is new P_One (Rec);

   begin  -- ONE
      null;
   end One;

   Result;
end C83027c;
