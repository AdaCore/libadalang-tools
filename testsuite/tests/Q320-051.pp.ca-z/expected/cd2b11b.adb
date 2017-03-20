-- CD2B11B.ADA

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
--     CHECK THAT IF A COLLECTION SIZE IS SPECIFIED FOR AN
--     ACCESS TYPE IN A GENERIC UNIT, THEN OPERATIONS ON VALUES OF THE
--     ACCESS TYPE ARE NOT AFFECTED.

-- HISTORY:
--     BCB 09/23/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report; use Report;

procedure Cd2b11b is

   Basic_Size : constant := 1_024;
   B          : Boolean;

begin

   Test
     ("CD2B11B",
      "CHECK THAT IF A COLLECTION SIZE IS SPECIFIED " &
      "FOR AN ACCESS TYPE, THEN " &
      "OPERATIONS ON VALUES OF THE ACCESS TYPE ARE " &
      "NOT AFFECTED");

   declare

      generic
      function Func return Boolean;

      function Func return Boolean is

         type Maintype is array (Integer range <>) of Integer;
         type Acc_Type is access Maintype;
         subtype Acc_Range is Acc_Type (1 .. 3);

         for Acc_Type'Storage_Size use Basic_Size;

         type Record_Type is record
            Comp : Acc_Type;
         end record;

         Check_Type1 : Acc_Type;
         Check_Type2 : Acc_Type;
         Check_Type3 : Acc_Type (1 .. 3);

         Check_Array : array (1 .. 3) of Acc_Type;

         Check_Record1 : Record_Type;
         Check_Record2 : Record_Type;

         Check_Param1 : Acc_Type;
         Check_Param2 : Acc_Type;

         Check_Null : Acc_Type := null;

         procedure Proc (Acc1, Acc2 : in out Acc_Type) is

         begin

            if (Acc1.all /= Acc2.all) then
               Failed ("INCORRECT VALUES FOR DESIGNATED " & "OBJECTS - 1");
            end if;

            if Equal (3, 3) then
               Acc2 := Acc1;
            end if;

            if Acc2 /= Acc1 then
               Failed ("INCORRECT RESULTS FOR RELATIONAL " & "OPERATORS - 1");
            end if;

            if (Acc1 in Acc_Range) then
               Failed ("INCORRECT RESULTS FOR " & "MEMBERSHIP TEST - 1");
            end if;

         end Proc;

      begin -- FUNC.

         Check_Param1 := new Maintype'(25, 35, 45);
         Check_Param2 := new Maintype'(25, 35, 45);

         Proc (Check_Param1, Check_Param2);

         if Acc_Type'Storage_Size < Basic_Size then
            Failed ("INCORRECT VALUE FOR ACCESS TYPE STORAGE_SIZE");
         end if;

         Check_Type1 := new Maintype'(25, 35, 45);
         Check_Type2 := new Maintype'(25, 35, 45);
         Check_Type3 := new Maintype'(1 => 1, 2 => 2, 3 => 3);

         Check_Array (1) := new Maintype'(25, 35, 45);
         Check_Array (2) := new Maintype'(25, 35, 45);

         Check_Record1.Comp := new Maintype'(25, 35, 45);
         Check_Record2.Comp := new Maintype'(25, 35, 45);

         if (Check_Type1.all /= Check_Type2.all) then
            Failed ("INCORRECT VALUES FOR DESIGNATED OBJECTS - 2");
         end if;

         if Equal (3, 3) then
            Check_Type2 := Check_Type1;
         end if;

         if Check_Type2 /= Check_Type1 then
            Failed ("INCORRECT RESULTS FOR RELATIONAL OPERATORS " & "- 2");
         end if;

         if (Check_Type1 in Acc_Range) then
            Failed ("INCORRECT RESULTS FOR MEMBERSHIP TEST - 2");
         end if;

         if (Check_Array (1).all /= Check_Array (2).all) then
            Failed ("INCORRECT VALUES FOR DESIGNATED OBJECTS - 3");
         end if;

         if Equal (3, 3) then
            Check_Array (2) := Check_Array (1);
         end if;

         if Check_Array (2) /= Check_Array (1) then
            Failed ("INCORRECT RESULTS FOR RELATIONAL OPERATORS " & "- 3");
         end if;

         if (Check_Array (1) in Acc_Range) then
            Failed ("INCORRECT RESULTS FOR MEMBERSHIP TEST - 3");
         end if;

         if (Check_Record1.Comp.all /= Check_Record2.Comp.all) then
            Failed ("INCORRECT VALUES FOR DESIGNATED OBJECTS - 4");
         end if;

         if Equal (3, 3) then
            Check_Record2 := Check_Record1;
         end if;

         if Check_Record2 /= Check_Record1 then
            Failed ("INCORRECT RESULTS FOR RELATIONAL OPERATORS " & "- 4");
         end if;

         if (Check_Record1.Comp in Acc_Range) then
            Failed ("INCORRECT RESULTS FOR MEMBERSHIP TEST - 4");
         end if;

         if Check_Type3'First /= Ident_Int (1) then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE3'FIRST");
         end if;

         if Check_Type3'Last /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE3'LAST");
         end if;

         return True;

      end Func;

      function Newfunc is new Func;

   begin
      B := Newfunc;
   end;

   Result;
end Cd2b11b;
