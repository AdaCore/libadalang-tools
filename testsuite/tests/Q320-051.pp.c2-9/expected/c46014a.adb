-- C46014A.ADA

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
--     FOR PREDEFINED TYPE INTEGER, CHECK THAT
--     CONSTRAINT_ERROR IS RAISED IF THE OPERAND VALUE OF A
--     CONVERSION LIES OUTSIDE OF THE RANGE OF THE TARGET TYPE'S BASE
--     TYPE. ALSO, CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE
--     OPERAND VALUE LIES OUTSIDE OF THE RANGE OF THE TARGET TYPE'S
--     SUBTYPE BUT WITHIN THE RANGE OF THE BASE TYPE.

-- HISTORY:
--     RJW 09/08/86  CREATED ORIGINAL TEST.
--     RJW 11/13/87  ADDED CODE TO PREVENT DEAD VARIABLE OPTIMIZATION.
--     JET 12/30/87  ADDED MORE CODE TO PREVENT OPTIMIZATION.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY
--     JRL 12/08/96  Changed usages of System.Max_Int and System.Min_Int to
--                   Integer'Base'Last and Integer'Base'First in first two
--                   subtests.

with Report; use Report;
procedure C46014a is

   subtype Small is Integer range -100 .. 100;
   S1 : Small;

   type Int is range -100 .. 100;
   T1 : Int;

   type Newinteger is new Integer;
   N1 : Newinteger;

   subtype Subnew is Newinteger range -100 .. 100;
   Sn : Subnew;

   I1 : Integer;
   P1 : Positive;
   L1 : Natural;

   function Ident (I : Integer) return Int is
   begin
      return Int'Val (Ident_Int (I));
   end Ident;

   function Ident (I : Newinteger) return Newinteger is
   begin
      return Newinteger'Val (Ident_Int (Newinteger'Pos (I)));
   end Ident;

begin
   Test
     ("C46014A",
      "FOR PREDEFINED TYPE INTEGER, CHECK THAT " &
      "CONSTRAINT_ERROR IS RAISED IF " &
      "THE OPERAND VALUE OF A CONVERSION LIES " &
      "OUTSIDE OF THE RANGE OF THE TARGET TYPE'S " &
      "BASE TYPE. ALSO, CHECK THAT " &
      "CONSTRAINT_ERROR IS RAISED IF THE OPERAND " &
      "VALUE LIES OUTSIDE OF THE RANGE OF THE " &
      "TARGET TYPE'S SUBTYPE BUT WITHIN THE " &
      "RANGE OF THE BASE TYPE");

   begin
      I1 := Integer'Base'Last + Ident_Int (1);
      Failed ("NO EXCEPTION RAISED FOR INTEGER'BASE'LAST + 1");
      if Equal (I1, I1) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         Comment ("CONSTRAINT_ERROR RAISED FOR INTEGER'BASE'LAST + 1");
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR INTEGER'BASE'LAST + 1");
   end;

   begin
      I1 := Integer'Base'First - Ident_Int (1);
      Failed ("NO EXCEPTION RAISED FOR INTEGER'BASE'FIRST - 1");
      if Equal (I1, I1) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         Comment ("CONSTRAINT_ERROR RAISED FOR INTEGER'BASE'FIRST - 1");
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR INTEGER'BASE'FIRST - 1");
   end;

   begin
      I1 := Integer (Ident_Int (Integer'First) - 1);
      Failed
        ("NO EXCEPTION RAISED FOR " &
         "INTEGER (IDENT_INT (INTEGER'FIRST) - 1)");
      if Equal (I1, I1) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         Comment
           ("CONSTRAINT_ERROR RAISED FOR " &
            "INTEGER (IDENT_INT (INTEGER'FIRST - 1)");
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR " &
            "INTEGER (IDENT_INT (INTEGER'FIRST - 1)");
   end;

   begin
      N1 := Newinteger (Ident_Int (Integer'Last) + 1);
      Failed
        ("NO EXCEPTION RAISED FOR " &
         "NEWINTEGER (IDENT_INT (INTEGER'LAST) + 1)");
      if Equal (Integer (N1), Integer (N1)) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         Comment
           ("CONSTRAINT_ERROR RAISED FOR " &
            "NEWINTEGER (IDENT_INT (INTEGER'LAST + 1)");
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR " &
            "NEWINTEGER (IDENT_INT (INTEGER'LAST + 1)");
   end;

   begin
      T1 := Int (Int'Base'First - Ident (1));
      Failed ("NO EXCEPTION RAISED FOR " & "INT (INT'BASE'FIRST - IDENT (1))");
      if Equal (Integer (T1), Integer (T1)) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         Comment
           ("CONSTRAINT_ERROR RAISED FOR " &
            "INT (INT'BASE'FIRST - IDENT (1))");
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR " &
            "INT (INT'BASE'FIRST - IDENT (1))");
   end;

   begin
      T1 := Ident (-101);
      Failed ("NO EXCEPTION RAISED FOR " & "T1 := -101");
      if Equal (Integer (T1), Integer (T1)) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR " & "T1 := -101");
   end;

   begin
      T1 := Integer'Pos (Ident_Int (101));
      Failed
        ("NO EXCEPTION RAISED FOR " & "T1 := INTEGER'POS (IDENT_INT (101))");
      if Equal (Integer (T1), Integer (T1)) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR " &
            "T1 := INTEGER'POS (IDENT_INT (101));");
   end;

   begin
      T1 := Int (Ident (Integer (Int'First)) - 1);
      Failed ("NO EXCEPTION RAISED FOR " & "INT (INT'FIRST - 1)");
      if Equal (Integer (T1), Integer (T1)) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR " & "INT (INT'FIRST - 1)");
   end;

   begin
      T1 := Int (Ident_Int (101));
      Failed ("NO EXCEPTION RAISED FOR INT (101)");
      if Equal (Integer (T1), Integer (T1)) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR INT (101)");
   end;

   begin
      S1 := Small (Ident_Int (101));
      Failed ("NO EXCEPTION RAISED FOR SMALL (101)");
      if Equal (S1, S1) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR SMALL (101)");
   end;

   begin
      Sn := Subnew (Ident_Int (-101));
      Failed ("NO EXCEPTION RAISED FOR SUBNEW (-101)");
      if Equal (Integer (Sn), Integer (Sn)) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR SUBNEW (-101)");
   end;

   begin
      P1 := Ident_Int (101);
      Sn := Subnew (P1);
      Failed ("NO EXCEPTION RAISED FOR SUBNEW (P1)");
      if Equal (Integer (Sn), Integer (Sn)) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR SUBNEW (P1)");
   end;

   begin
      Sn := Ident (0);
      P1 := Positive (Sn);
      Failed ("NO EXCEPTION RAISED FOR " & "POSITIVE (SN)");
      if Equal (P1, P1) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR " & "POSITIVE (SN)");
   end;

   begin
      N1 := Ident (-1);
      L1 := Natural (N1);
      Failed ("NO EXCEPTION RAISED FOR " & "NATURAL (N1)");
      if Equal (L1, L1) then
         Comment ("SHOULDN'T GET HERE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED FOR " & "NATURAL (N1)");
   end;

   Result;
end C46014a;
