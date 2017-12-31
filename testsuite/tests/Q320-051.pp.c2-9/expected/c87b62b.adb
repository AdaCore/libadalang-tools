-- C87B62B.ADA

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
--     CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
--       IN A LENGTH CLAUSE THAT SPECIFIES 'STORAGE_SIZE,
--       THE EXPRESSION MUST BE OF SOME INTEGER TYPE.
--       ACCESS TYPES ARE HERE; TASK TYPES ARE IN C87B62D.DEP.

-- HISTORY:
--     TRH 09/08/82  CREATED ORIGINAL TEST.
--     EG  06/04/84
--     PWB 01/19/86  CLARIFIED COMMENTS REGARDING NON-APPLICABILITY;
--                   REMOVED TEXT NOT RELATED TO TEST OBJECTIVE
--                   MOVED TASK TYPES TO C87B62D.DEP.
--     BCB 01/04/88  MODIFIED HEADER.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report; use Report;

procedure C87b62b is

   type Pos_Fix is delta 0.1 range 0.0 .. 10.0;
   type Pos_Int is new Integer range 0 .. Integer'Last;
   type Numeral is new Character range '0' .. '9';
   type Base_5 is ('0', '1', '2', '3', '4');
   Err : Boolean := False;

   function F (X : Integer) return Numeral is
   begin
      Err := True;
      return ('9');
   end F;

   function F (X : Integer) return Base_5 is
   begin
      Err := True;
      return ('4');
   end F;

   function F (X : Integer) return Pos_Fix is
   begin
      Err := True;
      return Pos_Fix (X);
   end F;

   function F (X : Integer) return Pos_Int is
   begin
      return Pos_Int (X);
   end F;

begin
   Test
     ("C87B62B",
      "OVERLOADED EXPRESSION WITHIN LENGTH CLAUSE " &
      "- SPECIFICATION OF ATTRIBUTE T'STORAGE_SIZE " & "FOR ACCESS TYPES");

   declare

      type Decem is new Integer range 1 .. 10;
      type Link is access Decem;

      type Just_Like_Link is access Decem;
      type Check is access Decem;

      for Check'Storage_Size use 1_024;
      for Link'Storage_Size use F (1_024);

   begin
      if Err then
         Failed
           ("RESOLUTION INCORRECT FOR EXPRESSION IN " &
            "LENGTH CLAUSE USING 'STORAGE_SIZE");
      end if;
   end;

   Result;
end C87b62b;
