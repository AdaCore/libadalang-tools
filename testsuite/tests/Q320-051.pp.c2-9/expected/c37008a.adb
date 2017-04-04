-- C37008A.ADA

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
-- CHECK THAT SPECIFYING AN INVALID DEFAULT INITIALIZATION RAISES
-- CONSTRAINT_ERROR WHEN AN OBJECT IS DECLARED.

-- DAT 3/6/81
-- SPS 10/26/82
-- RJW 1/9/86 - REVISED COMMENTS. ADDED 'IDENT_INT'. EDS 7/22/98 AVOID
-- OPTIMIZATION

with Report; use Report;
procedure C37008a is
begin
   Test
     ("C37008A",
      "CHECK THAT INVALID DEFAULT RECORD" &
      " COMPONENT INITIALIZATIONS RAISE" &
      " CONSTRAINT_ERROR");

   begin
      declare
         type R1 is record
            C1 : Integer range 1 .. 5 := Ident_Int (0);
         end record;
         Rec1 : R1;
      begin
         Failed ("NO EXCEPTION RAISED 1 " & Integer'Image (Rec1.C1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 1");
   end;

   begin
      declare
         type R is record
            C : Character range 'A' .. 'Y' := 'Z';
         end record;
         Rec2 : R;
      begin
         Failed ("NO EXCEPTION RAISED 1A " & (Rec2.C));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 1A");
   end;

   begin
      declare
         type R2 is record
            C2 : Boolean range False .. False := True;
         end record;
         Rec3 : R2;
      begin
         Failed ("NO EXCEPTION RAISED 2 " & Boolean'Image (Rec3.C2));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 2");
   end;

   begin
      declare
         type E is (E1, E2, E3);
         type R is record
            C : E range E2 .. E3 := E1;
         end record;
         Rec4 : R;
      begin
         Failed ("NO EXCEPTION RAISED 2A " & E'Image (Rec4.C));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 2A");
   end;

   begin
      declare
         type R3 is record
            C3 : Integer range 1 .. 5;
         end record;
         Rec5 : R3;
         type R3a is record
            C3a : R3 := (others => Ident_Int (6));
         end record;
         Rec6 : R3a;
      begin
         Failed ("NO EXCEPTION RAISED 3 " & Integer'Image (Rec6.C3a.C3));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 3");
   end;

   begin
      declare
         type Arr is array (1 .. 3) of Integer range 8 .. 9;
         type R4 is record
            C4 : Arr := (1 => 8, 2 => 9, 3 => 10);
         end record;
         Rec7 : R4;
      begin
         Failed ("NO EXCEPTION RAISED 4 " & Integer'Image (Rec7.C4 (1)));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 4");
   end;

   begin
      declare
         type A is array (Natural range <>) of Integer range 1 .. 5;

         type Aa is access A;

         type R5 is record
            C5 : Aa := new A'(4, 5, 6);
         end record;
         Rec8 : R5;
      begin
         Failed ("NO EXCEPTION RAISED 5 " & Integer'Image (Rec8.C5 (1)));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 5");
   end;

   begin
      declare
         type A is array (Natural range <>) of Integer range 1 .. 5;

         type Aa is access A (1 .. 3);

         type R6 is record
            C6 : Aa := new A'(4, 4, 4, 4);
         end record;
         Rec9 : R6;
      begin
         Failed ("NO EXCEPTION RAISED 6 " & Integer'Image (Rec9.C6 (1)));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 6");
   end;

   begin
      declare
         type Ai is access Integer range 6 .. 8;

         type R7 is record
            C7 : Ai := new Integer'(5);
         end record;
         Rec10 : R7;
      begin
         Failed ("NO EXCEPTION RAISED 7 " & Integer'Image (Rec10.C7.all));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 7");
   end;

   begin
      declare
         type Ua is array (Natural range <>) of Integer range 3 .. 5;

         subtype Ca is Ua (7 .. 8);

         type R8 is record
            C8 : Ca := (6 .. 8 => 4);
         end record;
         Rec11 : R8;
      begin
         Failed ("NO EXCEPTION RAISED 8 " & Integer'Image (Rec11.C8 (7)));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 8");
   end;

   begin
      declare
         type Ua is
           array (Natural range <>) of Integer range 3 .. Ident_Int (5);

         type R9 is record
            C9 : Ua (11 .. 11) := (11 => 6);
         end record;
         Rec12 : R9;
      begin
         Failed ("NO EXCEPTION RAISED 9 " & Integer'Image (Rec12.C9 (11)));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 9");
   end;

   begin
      declare
         type A is
           array (Natural range <>) of Integer range 1 .. Ident_Int (5);

         type Aa is access A;

         type R10 is record
            C10 : Aa := new A'(4, 5, 6);
         end record;
         Rec13 : R10;
      begin
         Failed ("NO EXCEPTION RAISED 10 " & Integer'Image (Rec13.C10 (1)));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 10");
   end;

   begin
      declare
         type A is array (Natural range <>) of Integer range 1 .. 5;

         type Aa is access A (Ident_Int (1) .. Ident_Int (3));

         type R11 is record
            C11 : Aa := new A'(4, 4, 4, 4);
         end record;
         Rec14 : R11;
      begin
         Failed ("NO EXCEPTION RAISED 11 " & Integer'Image (Rec14.C11 (1)));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 11");
   end;

   Result;
end C37008a;
