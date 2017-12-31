-- C64201C.ADA

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
-- CHECK THAT INITIALIZATION OF IN PARAMETERS OF A COMPOSITE
--   TYPE HAVING AT LEAST ONE COMPONENT (INCLUDING COMPONENTS
--   OF COMPONENTS) OF A TASK TYPE IS PERMITTED.
--  (SEE ALSO 7.4.4/T2 FOR TESTS OF LIMITED PRIVATE TYPES.)

-- CVP 5/14/81
-- ABW 7/1/82
-- BHS 7/9/84

with Report; use Report;
procedure C64201c is

   Global : Integer := 10;

   task type T is
      entry E (X : in out Integer);
   end T;

   type Rec_T is record
      Tt : T;
      Bb : Boolean := True;
   end record;

   type Rec_Rec_T is record
      Rr : Rec_T;
   end record;

   type Arr_T is array (1 .. 2) of T;

   type Arr_Rec_T is array (1 .. 2) of Rec_T;

   Rt1, Rt2   : Rec_T;
   Rrt1, Rrt2 : Rec_Rec_T;
   At1, At2   : Arr_T;
   Art1, Art2 : Arr_Rec_T;

   task body T is
   begin
      accept E (X : in out Integer) do
         X := X - 1;
      end E;
      accept E (X : in out Integer) do
         X := X + 1;
      end E;
   end T;

   procedure Proc1a (P1x : Rec_T := Rt1) is
   begin
      if P1x.Bb then                 -- EXPECT RT2 PASSED.
         Failed ("RECORD OF TASK NOT PASSED, DEFAULT EMPLOYED");
      end if;
   end Proc1a;

   procedure Proc1b (P1x : Rec_T := Rt1) is
   begin
      if not P1x.Bb then             -- EXPECT DEFAULT USED.
         Failed ("DEFAULT RECORD OF TASK NOT EMPLOYED");
      end if;
   end Proc1b;

   procedure Proc2a (P2x : Rec_Rec_T := Rrt1) is
   begin
      if P2x.Rr.Bb then             -- EXPECT RRT2 PASSED.
         Failed ("RECORD OF RECORD OF TASK NOT PASSED, " & "DEFAULT EMPLOYED");
      end if;
   end Proc2a;

   procedure Proc2b (P2x : Rec_Rec_T := Rrt1) is
   begin
      if not P2x.Rr.Bb then         -- EXPECT DEFAULT USED.
         Failed ("DEFAULT RECORD OF RECORD OF TASK " & "NOT EMPLOYED");
      end if;
   end Proc2b;

   procedure Proc3 (P3x : Arr_T := At1) is
   begin
      P3x (1).E (X => Global);        -- CALL TO AT2(1).E,
      -- GLOBAL => GLOBAL - 1.
   end Proc3;

   procedure Proc4 (P4x : Arr_T := At1) is
   begin
      P4x (1).E (X => Global);     -- CALL TO DEFAULT AT1(1).E,
      -- GLOBAL => GLOBAL - 1.
      if Global /= Ident_Int (8) then
         Failed ("ARRAY OF TASKS NOT PASSED " & "CORRECTLY IN PROC3");
      end if;
   end Proc4;

   procedure Proc5 (P5x : Arr_Rec_T := Art1) is
   begin
      P5x (1).Tt.E (X => Global);      -- CALL TO ART2(1).TT.E,
      -- GLOBAL => GLOBAL - 1.
   end Proc5;

   procedure Proc6 (P6x : Arr_Rec_T := Art1) is
   begin
      P6x (1).Tt.E (X => Global);      -- CALL DEFAULT ART1(1).TT.E,
      -- GLOBAL => GLOBAL - 1.
      if Global /= Ident_Int (8) then
         Failed ("ARRAY OF RECORDS OF TASKS NOT " & "PASSED IN PROC5");
      end if;
   end Proc6;

   procedure Term (Tsk : T; Num : Character) is
   begin
      if not Tsk'Terminated then
         abort Tsk;
         Comment ("ABORTING TASK " & Num);
      end if;
   end Term;

begin

   Test
     ("C64201C",
      "CHECK THAT INITIALIZATION OF IN " & "PARAMETERS OF A COMPOSITE TYPE " &
      "IS PERMITTED");

   Rt2.Bb     := False;
   Rrt2.Rr.Bb := False;

   Proc1a (Rt2);                               -- NO ENTRY CALL
   Proc1b;                                    -- NO ENTRY CALL
   Proc2a (Rrt2);                              -- NO ENTRY CALL
   Proc2b;                                    -- NO ENTRY CALL

   Proc3 (At2);                                -- CALL AT2(1).E
   if Global /= 9 then
      Failed ("INCORRECT GLOBAL VALUE AFTER PROC3");
   else
      Proc4;                                -- CALL AT1(1).E
   end if;

   Global := 10;
   Proc5 (Art2);                              -- CALL ART2(1).TT.E
   if Global /= 9 then
      Failed ("INCORRECT GLOBAL VALUE AFTER PROC5");
   else
      Proc6;                               -- CALL ART1(1).TT.E
   end if;

-- MAKE SURE ALL TASKS TERMINATED
   Term (Rt1.Tt, '1');
   Term (Rt2.Tt, '2');
   Term (Rrt1.Rr.Tt, '3');
   Term (Rrt2.Rr.Tt, '4');
   Term (At1 (1), '5');
   Term (At2 (1), '6');
   Term (At1 (2), '7');
   Term (At2 (2), '8');
   Term (Art1 (1).Tt, '9');
   Term (Art2 (1).Tt, 'A');
   Term (Art1 (2).Tt, 'B');
   Term (Art2 (2).Tt, 'C');

   Result;

end C64201c;
