-- C37217B.ADA

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
--     CHECK WHETHER THE OPTIONAL COMPATIBILITY CHECK IS
--     PERFORMED WHEN A DISCRIMINANT CONSTRAINT IS GIVEN FOR AN ACCESS
--     TYPE - BEFORE THE DESIGNATED TYPE'S FULL DECLARATION.

-- HISTORY:
--     DHH 08/04/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C37217b is

   subtype Sm is Integer range 1 .. 10;

begin  --C37217B BODY
   Test
     ("C37217B",
      "CHECK WHETHER THE OPTIONAL COMPATIBILITY " &
      "CHECK IS PERFORMED WHEN A DISCRIMINANT " &
      "CONSTRAINT IS GIVEN FOR AN ACCESS TYPE - " &
      "BEFORE THE DESIGNATED TYPE'S FULL DECLARATION");

---------------------------------------------------------------------
   -- INCOMPLETE DECLARATION
   -- UPPER LIMIT
   begin  -- F
      declare  -- F
         type Rec (D1 : Integer);

         type Ptr is access Rec;
         X : Ptr (Ident_Int (11));

         type Sm_Rec (D : Sm) is record
            null;
         end record;

         type Rec (D1 : Integer) is record
            Int : Sm_Rec (D1);
         end record;
      begin
         Comment ("OPTIONAL COMPATIBILITY CHECK NOT PERFORMED " & "- UPPER");
         X := new Rec (Ident_Int (11));
         Failed ("CONSTRAINT ERROR NOT RAISED - UPPER");

         if Ident_Int (X.Int.D) /= Ident_Int (1) then
            Comment ("IRREVELANT");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("UNEXPECTED EXCEPTION RAISED IN " &
               "VARIABLE USE - INCOMPLETE UPPER");
      end;
   exception
      when Constraint_Error =>
         Comment
           ("OPTIONAL COMPATIBILITY CHECK PERFORMED " & "- INCOMPLETE UPPER");
      when others =>
         Failed
           ("UNEXPECTED EXCEPTION RAISED IN " &
            "VARIABLE DECLARATION - INCOMPLETE UPPER");
   end;  -- F

-----------------------------------------------------------------------
   -- INCOMPLETE DECLARATION
   -- LOWER LIMIT
   begin  -- A
      declare  -- A
         type Rec (D1 : Integer);

         type Ptr is access Rec;
         X : Ptr (Ident_Int (0));

         type Sm_Arr is array (Sm range <>) of Integer;

         type Rec (D1 : Integer) is record
            Int : Sm_Arr (D1 .. 2);
         end record;
      begin
         Comment ("OPTIONAL COMPATIBILITY CHECK NOT PERFORMED " & "- LOWER");
         X := new Rec'(Ident_Int (0), Int => (others => Ident_Int (1)));
         Failed ("CONSTRAINT ERROR NOT RAISED - LOWER");

         if X.Int (Ident_Int (1)) /= Ident_Int (1) then
            Comment ("IRREVELANT");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("UNEXPECTED EXCEPTION RAISED IN " &
               "VARIABLE USE - INCOMPLETE LOWER");
      end;
   exception
      when Constraint_Error =>
         Comment
           ("OPTIONAL COMPATIBILITY CHECK PERFORMED " & "- INCOMPLETE LOWER");
      when others =>
         Failed
           ("UNEXPECTED EXCEPTION RAISED IN " &
            "VARIABLE DECLARATION - INCOMPLETE LOWER");
   end;
-----------------------------------------------------------------------
   Result;

end C37217b;  -- BODY
