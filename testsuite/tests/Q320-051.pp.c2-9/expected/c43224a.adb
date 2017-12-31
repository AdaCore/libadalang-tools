-- C43224A.ADA

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
--     CHECK THAT A NON-STATIC CHOICE OF AN ARRAY AGGREGATE CAN BE A
--     'RANGE ATTRIBUTE.

-- HISTORY:
--     DHH 08/15/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43224a is

   M, O : Integer := Ident_Int (2);
   N    : Integer := Ident_Int (3);

   type Arr is array (Integer range <>) of Integer;
   type D3_Arr is
     array (Integer range <>, Integer range <>, Integer range <>) of Integer;

   subtype Arr1 is Arr (Ident_Int (2) .. Ident_Int (3));
   subtype Arr2 is D3_Arr (1 .. M, 1 .. N, 1 .. O);

   Sub  : Arr1;
   Sub1 : Arr2;

   procedure Proc (Arry : in out Arr) is
   begin
      Arry := (Arr1'Range => Ident_Int (7));
      if Arry (Ident_Int (Arry'First)) /= Ident_Int (7) then
         Failed ("RANGE NOT INITIALIZED - 1");
      end if;
   end Proc;

   procedure Proc1 (Arry : in out D3_Arr) is
   begin
      Arry :=
        (Arr2'Range (1) =>
           (Arry'Range (2) => (Arry'Range (3) => Ident_Int (7))));

      if Arry (Ident_Int (1), Ident_Int (2), Ident_Int (1)) /= Ident_Int (7)
      then
         Failed ("RANGE NOT INITIALIZED - 2");
      end if;
   end Proc1;

begin
   Test
     ("C43224A",
      "CHECK THAT A NON-STATIC CHOICE OF AN ARRAY " &
      "AGGREGATE CAN BE A 'RANGE ATTRIBUTE");

   Proc (Sub);
   Proc1 (Sub1);

   Result;
end C43224a;
