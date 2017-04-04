-- C38005A.ADA

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
-- CHECK THAT ALL (UNINITIALIZED) ACCESS OBJECTS ARE INITIALIZED TO NULL BY
-- DEFAULT. VARIABLES, ARRAYS, RECORDS, ARRAYS OF RECORDS, ARRAYS OF ARRAYS,
-- RECORDS WITH ARRAYS AND RECORD COMPONENTS ARE ALL CHECKED. FUNCTION RESULTS
-- (I.E. RETURNED FROM IMPLICIT FUNCTION RETURN) ARE NOT CHECKED.

-- DAT 3/6/81
-- VKG 1/5/83
-- SPS 2/17/83

with Report; use Report;

procedure C38005a is

   type Rec;
   type Acc_Rec is access Rec;
   type Vector is array (Natural range <>) of Acc_Rec;
   type Rec is record
      Vect : Vector (3 .. 5);
   end record;

   type Acc_Vect is access Vector;
   type Arr_Rec is array (1 .. 2) of Rec;
   type Rec2;
   type Acc_Rec2 is access Rec2;
   type Rec2 is record
      C1 : Acc_Rec;
      C2 : Acc_Vect;
      C3 : Arr_Rec;
      C4 : Rec;
      C5 : Acc_Rec2;
   end record;

   N_Rec      : Rec;
   N_Acc_Rec  : Acc_Rec;
   N_Vec      : Vector (3 .. Ident_Int (5));
   N_Acc_Vect : Acc_Vect;
   N_Arr_Rec  : Arr_Rec;
   N_Rec2     : Rec2;
   N_Acc_Rec2 : Acc_Rec2;
   N_Arr      : array (1 .. 2) of Vector (1 .. 2);
   Q          : Rec2 :=
     (C1 => new Rec,
      C2 => new Vector'(new Rec, new Rec'(N_Rec)),
      C3 => (1 | 2 => (Vect => (3 | 4 => new Rec, 5 => N_Acc_Rec))),
      C4 => N_Rec2.C4,
      C5 => new Rec2'(N_Rec2));

begin
   Test ("C38005A", "DEFAULT VALUE FOR ACCESS OBJECTS IS NULL");

   if N_Rec /= Rec'(Vect => (3 .. 5 => null)) then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 1");
   end if;

   if N_Acc_Rec /= null then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 2");
   end if;

   if N_Vec /= N_Rec.Vect then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 3");
   end if;

   if N_Arr /= ((null, null), (null, null)) then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 4");
   end if;

   if N_Acc_Vect /= null then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 5");
   end if;

   if N_Arr_Rec /= (N_Rec, N_Rec) then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 6");
   end if;

   if N_Rec2 /= (null, null, N_Arr_Rec, N_Rec, null) then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 7");
   end if;

   if N_Acc_Rec2 /= null then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 8");
   end if;

   if Q /= (Q.C1, Q.C2, (Q.C3 (1), Q.C3 (2)), N_Rec, Q.C5) then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 9");
   end if;

   if Q.C1.all /= N_Rec then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 10");
   end if;

   if Q.C2.all (0).all /= N_Rec then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 11");
   end if;

   if Q.C2 (1).Vect /= N_Vec then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 12");
   end if;

   if Q.C3 (2).Vect /=
     (3 => Q.C3 (2).Vect (3), 4 => Q.C3 (2).Vect (4), 5 => null)
   then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 13");
   end if;

   if Q.C3 (2).Vect (3).all /= N_Rec then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 14");
   end if;

   if Q.C5.all /= N_Rec2 then
      Failed ("INCORRECT ACCESS TYPE INITIALIZATION - 15");
   end if;

   declare
      procedure T (R : out Rec2) is
      begin
         null;
      end T;
   begin
      N_Rec2 := Q;
      T (Q);
      if Q /= N_Rec2 then
         Failed ("INCORRECT OUT PARM INIT 2");
      end if;
   end;

   Result;
end C38005a;
