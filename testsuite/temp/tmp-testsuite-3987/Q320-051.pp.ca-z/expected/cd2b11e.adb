-- CD2B11E.ADA

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
--     CHECK THAT THE EXPRESSION IN A COLLECTION SIZE CLAUSE
--     FOR AN ACCESS TYPE IN A GENERIC UNIT NEED NOT BE STATIC.

-- HISTORY:
--     BCB 09/23/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report; use Report;

procedure Cd2b11e is

   B : Boolean;

begin

   Test
     ("CD2B11E",
      "CHECK THAT THE EXPRESSION IN A COLLECTION " &
      "SIZE CLAUSE FOR AN ACCESS TYPE IN A " &
      "GENERIC UNIT NEED NOT BE STATIC");

   declare

      generic
      function Func return Boolean;

      function Func return Boolean is

         type Test_Type is access Integer;
         for Test_Type'Storage_Size use 256;

         type Acc_Type is access Integer;
         for Acc_Type'Storage_Size use Ident_Int (256);

      begin -- FUNC.

         if Acc_Type'Storage_Size < Ident_Int (256) then
            Failed ("INCORRECT VALUE FOR STORAGE_SIZE");
         end if;

         return True;

      end Func;

      function Newfunc is new Func;

   begin
      B := Newfunc;
   end;

   Result;
end Cd2b11e;
