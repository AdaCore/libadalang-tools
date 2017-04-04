-- CD5011Q.ADA

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
--     PRIVATE TYPE IN THE DECLARATIVE PART OF A BLOCK STATEMENT.

-- HISTORY:
--     JET 09/15/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with System; use System;
with Report; use Report;
with Spprt13;

procedure Cd5011q is

   package P is
      type Priv_Type is private;
      function Int_To_Priv (I : Integer) return Priv_Type;
      function Equal (P : Priv_Type; I : Integer) return Boolean;
   private
      type Priv_Type is new Integer;
   end P;

   package body P is

      function Int_To_Priv (I : Integer) return Priv_Type is
      begin
         return Priv_Type (I);
      end Int_To_Priv;

      function Equal (P : Priv_Type; I : Integer) return Boolean is
      begin
         return (P = Priv_Type (I));
      end Equal;

   end P;

   use P;

begin

   Test
     ("CD5011Q",
      "AN ADDRESS CLAUSE CAN BE " &
      "GIVEN FOR A VARIABLE OF A PRIVATE " &
      "TYPE IN THE DECLARATIVE PART OF A " &
      "BLOCK STATEMENT");

   declare

      Priv : Priv_Type := Int_To_Priv (12);
      for Priv use at Spprt13.Variable_Address;

   begin
      Priv := Int_To_Priv (17);

      if not Equal (Priv, Ident_Int (17)) then
         Failed ("INCORRECT VALUE FOR VARIABLE OF PRIVATE TYPE");
      end if;

      if Priv'Address /= Spprt13.Variable_Address then
         Failed ("INCORRECT ADDRESS FOR VARIABLE OF " & "PRIVATE TYPE");
      end if;
   end;

   Result;

end Cd5011q;
