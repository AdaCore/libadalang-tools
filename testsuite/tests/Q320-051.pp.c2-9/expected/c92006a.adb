-- C92006A.ADA

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
-- CHECK THAT TASK OBJECTS CAN BE INTERCHANGED BY ASSIGNMENT OF CORRESPONDING
-- ACCESS TYPE OBJECTS.

-- WEI  3/ 4/82
-- JWC 6/28/85 RENAMED FROM C920BIA-B.ADA

with Report; use Report;
procedure C92006a is

   task type Tt1 is
      entry E1;
      entry E2;
   end Tt1;

   type Att1 is access Tt1;
   Pointer_Tt1_1, Pointer_Tt1_2 : Att1;

   subtype Arg is Natural range 0 .. 9;
   Spynumb : Natural := 0;

   procedure Pspy_Numb (Digt : in Arg) is
   begin
      Spynumb := 10 * Spynumb + Digt;
   end Pspy_Numb;

   procedure Proc (P1, P2 : in out Att1) is
      -- SWAP TASK OBJECTS P1, P2.
      Scratch : Att1;
   begin
      Scratch := P1;
      P1      := P2;
      P2      := Scratch;

      P1.E2;  -- ENTRY2 SECOND OBJECT.
      P2.E1;  -- VICE VERSA.

   end Proc;

   task body Tt1 is
   begin
      accept E1 do
         Pspy_Numb (1);
      end E1;
      accept E2 do
         Pspy_Numb (2);
      end E2;
   end Tt1;

begin

   Test ("C92006A", "INTERCHANGING TASK OBJECTS");
   Pointer_Tt1_1 := new Tt1;
   Pointer_Tt1_2 := new Tt1;

   Pointer_Tt1_2.all.E1;
   Proc (Pointer_Tt1_1, Pointer_Tt1_2);
   Pointer_Tt1_2.E2;        -- E2 OF FIRST OBJECT
-- EACH ENTRY OF EACH TASK OBJECT SHOULD HAVE BEEN CALLED.

   if Spynumb /= 1_212 then
      Failed ("FAILURE TO SWAP TASK OBJECTS " & "IN PROCEDURE PROC");
      Comment ("ACTUAL ORDER WAS:" & Integer'Image (Spynumb));
   end if;

   Result;

end C92006a;
