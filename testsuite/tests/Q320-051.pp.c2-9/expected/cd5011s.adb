-- CD5011S.ADA

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
--     CHECK THAT AN ADDRESS CLAUSE CAN BE GIVEN FOR A VARIABLE OF A
--     LIMITED PRIVATE TYPE IN THE DECLARATIVE PART OF A SUBPROGRAM.

-- HISTORY:
--     JET 09/16/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with System; use System;
with Report; use Report;
with Spprt13;

procedure Cd5011s is

   package P is
      type Limp_Type is limited private;
      procedure Test_Limp (Limp : in out Limp_Type);
   private
      type Limp_Type is array (1 .. 10) of Integer;
   end P;

   package body P is
      procedure Test_Limp (Limp : in out Limp_Type) is
      begin
         for I in Limp'Range loop
            Limp (I) := Ident_Int (I);
         end loop;

         for I in Limp'Range loop
            if Limp (I) /= I then
               Failed ("INCORRECT VALUE FOR ELEMENT" & Integer'Image (I));
            end if;
         end loop;
      end Test_Limp;
   end P;

   use P;

   procedure Cd5011s_Proc is

      Limp : Limp_Type;
      for Limp use at Spprt13.Variable_Address;

   begin
      Test_Limp (Limp);

      if Limp'Address /= Spprt13.Variable_Address then
         Failed ("WRONG ADDRESS FOR VARIABLE OF A LIMITED " & "PRIVATE TYPE");
      end if;
   end Cd5011s_Proc;

begin
   Test
     ("CD5011S",
      "AN ADDRESS CLAUSE CAN BE " &
      "GIVEN FOR A VARIABLE OF A LIMITED " &
      "PRIVATE TYPE IN THE DECLARATIVE PART " &
      "OF A SUBPROGRAM");

   Cd5011s_Proc;

   Result;

end Cd5011s;
