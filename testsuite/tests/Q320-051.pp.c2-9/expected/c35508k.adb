-- C35508K.ADA

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
-- CHECK THAT 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN THE PREFIX IS A
-- BOOLEAN TYPE.

-- RJW 3/19/86
-- PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C35508k is

   type Newbool is new Boolean;

begin
   Test
     ("C35508K",
      "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
      "CORRECT RESULTS  WHEN THE PREFIX IS A " & "BOOLEAN TYPE");

   begin
      if Boolean'Pos (Ident_Bool (False)) /= 0 then
         Failed ("WRONG POS FOR 'FALSE'");
      end if;
      if Boolean'Pos (Ident_Bool (True)) /= 1 then
         Failed ("WRONG POS FOR 'TRUE'");
      end if;

      if Boolean'Val (Ident_Int (0)) /= False then
         Failed ("WRONG VAL FOR '0'");
      end if;
      if Boolean'Val (Ident_Int (1)) /= True then
         Failed ("WRONG VAL FOR '1'");
      end if;
   end;

   begin
      if Boolean'Val (Ident_Int (-1)) = True then
         Failed ("'VAL(-1) WRAPPED AROUND TO TRUE");
      end if;
      Failed ("NO EXCEPTION RAISED FOR VAL OF '-1'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR VAL OF '-1'");
   end;

   begin
      if Boolean'Val (Ident_Int (2)) = False then
         Failed ("BOOLEAN'VAL(2) WRAPPED AROUND TO FALSE");
      end if;
      Failed ("NO EXCEPTION RAISED FOR VAL OF '2'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR VAL OF '2'");
   end;

   begin
      if Newbool'Pos (False) /= 0 then
         Failed ("WRONG POS FOR NEWBOOL'(FALSE)");
      end if;
      if Newbool'Pos (True) /= 1 then
         Failed ("WRONG POS FOR NEWBOOL'(TRUE)");
      end if;

      if Newbool'Val (0) /= False then
         Failed ("WRONG NEWBOOL'VAL FOR '0'");
      end if;
      if Newbool'Val (1) /= True then
         Failed ("WRONG NEWBOOL'VAL FOR '1'");
      end if;
   end;

   begin
      if Newbool'Val (Ident_Int (-1)) = True then
         Failed ("NEWBOOL'VAL(-1) WRAPPED AROUND TO TRUE");
      end if;
      Failed ("NO EXCEPTION RAISED FOR NEWBOOL'VAL OF '-1'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR " & "NEWBOOL'VAL OF '-1'");
   end;

   begin
      if Newbool'Val (Ident_Int (2)) = False then
         Failed ("NEWBOOL'VAL(2) WRAPPED AROUND TO FALSE");
      end if;
      Failed ("NO EXCEPTION RAISED FOR NEWBOOL'VAL OF '2'");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR " & "NEWBOOL'VAL OF '2'");
   end;

   Result;
end C35508k;
