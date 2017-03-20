-- C95008A.ADA

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
-- CHECK THAT THE EXCEPTION CONSTRAINT_ERROR IS RAISED FOR AN
--   OUT-OF-RANGE INDEX VALUE WHEN REFERENCING AN ENTRY FAMILY,
--   EITHER IN AN ACCEPT_STATEMENT OR IN AN ENTRY_CALL.

-- SUBTESTS ARE:
--   (A)  INTEGER TYPE, STATIC LOWER BOUND, NO PARAMETERS.
--   (B)  CHARACTER TYPE, DYNAMIC UPPER BOUND, NO PARAMETERS.
--   (C)  BOOLEAN TYPE, STATIC NULL RANGE, NO PARAMETERS.
--   (D)  USER-DEFINED ENUMERATED TYPE, DYNAMIC LOWER BOUND, ONE
--           PARAMETER.
--   (E)  DERIVED INTEGER TYPE, DYNAMIC NULL RANGE, ONE PARAMETER.
--   (F)  DERIVED USER-DEFINED ENUMERATED TYPE, STATIC UPPER BOUND,
--           ONE PARAMETER.

-- JRK 11/4/81
-- JBG 11/11/84
-- SAIC 11/14/95 fixed test for 2.0.1

with Report; use Report;
procedure C95008a is

   C_E_Not_Raised   : Boolean;
   Wrong_Exc_Raised : Boolean;

begin
   Test
     ("C95008A",
      "OUT-OF-RANGE ENTRY FAMILY INDICES IN " &
      "ACCEPT_STATEMENTS AND ENTRY_CALLS");

   --------------------------------------------------

   C_E_Not_Raised   := False;
   Wrong_Exc_Raised := False;

   declare -- (A)

      task T is
         entry E (1 .. 10);
         entry Continue;
      end T;

      task body T is
      begin
         accept Continue;
         select
            accept E (0);
         or
            delay 1.0;
         end select;
         C_E_Not_Raised := True;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Wrong_Exc_Raised := True;
      end T;

   begin -- (A)

      select
         T.E (0);
      or
         delay 15.0;
      end select;
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ENTRY_CALL - (A)");
      T.Continue;

   exception -- (A)

      when Constraint_Error =>
         T.Continue;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN " & "ENTRY_CALL - (A)");
         T.Continue;

   end; -- (A)

   if C_E_Not_Raised then
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ACCEPT_STATEMENT - (A)");
   end if;

   if Wrong_Exc_Raised then
      Failed ("WRONG EXCEPTION RAISED IN " & "ACCEPT_STATEMENT - (A)");
   end if;

   --------------------------------------------------

   C_E_Not_Raised   := False;
   Wrong_Exc_Raised := False;

   declare -- (B)

      task T is
         entry E (Character range 'A' .. 'Y');
         entry Continue;
      end T;

      task body T is
      begin
         accept Continue;
         select
            accept E (Ident_Char ('Z'));
         or
            delay 1.0;
         end select;
         C_E_Not_Raised := True;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Wrong_Exc_Raised := True;
      end T;

   begin -- (B)

      select
         T.E (Ident_Char ('Z'));
      or
         delay 15.0;
      end select;
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ENTRY_CALL - (B)");
      T.Continue;

   exception -- (B)

      when Constraint_Error =>
         T.Continue;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN " & "ENTRY_CALL - (B)");
         T.Continue;

   end; -- (B)

   if C_E_Not_Raised then
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ACCEPT_STATEMENT - (B)");
   end if;

   if Wrong_Exc_Raised then
      Failed ("WRONG EXCEPTION RAISED IN " & "ACCEPT_STATEMENT - (B)");
   end if;

   --------------------------------------------------

   C_E_Not_Raised   := False;
   Wrong_Exc_Raised := False;

   declare -- (C)

      task T is
         entry E (True .. False);
         entry Continue;
      end T;

      task body T is
      begin
         accept Continue;
         select
            accept E (False);
         or
            delay 1.0;
         end select;
         C_E_Not_Raised := True;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Wrong_Exc_Raised := True;
      end T;

   begin -- (C)

      select
         T.E (True);
      or
         delay 15.0;
      end select;
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ENTRY_CALL - (C)");
      T.Continue;

   exception -- (C)

      when Constraint_Error =>
         T.Continue;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN " & "ENTRY_CALL - (C)");
         T.Continue;

   end; -- (C)

   if C_E_Not_Raised then
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ACCEPT_STATEMENT - (C)");
   end if;

   if Wrong_Exc_Raised then
      Failed ("WRONG EXCEPTION RAISED IN " & "ACCEPT_STATEMENT - (C)");
   end if;

   --------------------------------------------------

   C_E_Not_Raised   := False;
   Wrong_Exc_Raised := False;

   declare -- (D)

      type Et is (E0, E1, E2);
      Dlb : Et := Et'Val (Ident_Int (1));      -- E1.

      task T is
         entry E (Et range Dlb .. E2) (I : Integer);
         entry Continue;
      end T;

      task body T is
      begin
         accept Continue;
         select
            accept E (E0) (I : Integer);
         or
            delay 1.0;
         end select;
         C_E_Not_Raised := True;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Wrong_Exc_Raised := True;
      end T;

   begin -- (D)

      select
         T.E (E0) (0);
      or
         delay 15.0;
      end select;
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ENTRY_CALL - (D)");
      T.Continue;

   exception -- (D)

      when Constraint_Error =>
         T.Continue;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN " & "ENTRY_CALL - (D)");
         T.Continue;

   end; -- (D)

   if C_E_Not_Raised then
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ACCEPT_STATEMENT - (D)");
   end if;

   if Wrong_Exc_Raised then
      Failed ("WRONG EXCEPTION RAISED IN " & "ACCEPT_STATEMENT - (D)");
   end if;

   --------------------------------------------------

   C_E_Not_Raised   := False;
   Wrong_Exc_Raised := False;

   declare -- (E)

      type D_I is new Integer;
      subtype Di is D_I range 3 .. D_I (Ident_Int (2));

      task T is
         entry E (Di) (I : Integer);
         entry Continue;
      end T;

      task body T is
      begin
         accept Continue;
         select
            accept E (D_I (3)) (I : Integer);
         or
            delay 1.0;
         end select;
         C_E_Not_Raised := True;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Wrong_Exc_Raised := True;
      end T;

   begin -- (E)

      select
         T.E (D_I (2)) (0);
      or
         delay 15.0;
      end select;
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ENTRY_CALL - (E)");
      T.Continue;

   exception -- (E)

      when Constraint_Error =>
         T.Continue;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN " & "ENTRY_CALL - (E)");
         T.Continue;

   end; -- (E)

   if C_E_Not_Raised then
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ACCEPT_STATEMENT - (E)");
   end if;

   if Wrong_Exc_Raised then
      Failed ("WRONG EXCEPTION RAISED IN " & "ACCEPT_STATEMENT - (E)");
   end if;

   --------------------------------------------------

   C_E_Not_Raised   := False;
   Wrong_Exc_Raised := False;

   declare -- (F)

      type Et is (E0, E1, E2);
      type D_Et is new Et;

      task T is
         entry E (D_Et range E0 .. E1) (I : Integer);
         entry Continue;
      end T;

      task body T is
      begin
         accept Continue;
         select
            accept E (D_Et'(E2)) (I : Integer);
         or
            delay 1.0;
         end select;
         C_E_Not_Raised := True;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Wrong_Exc_Raised := True;
      end T;

   begin -- (F)

      select
         T.E (D_Et'(E2)) (0);
      or
         delay 15.0;
      end select;
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ENTRY_CALL - (F)");
      T.Continue;

   exception -- (F)

      when Constraint_Error =>
         T.Continue;
      when others =>
         Failed ("WRONG EXCEPTION RAISED IN " & "ENTRY_CALL - (F)");
         T.Continue;

   end; -- (F)

   if C_E_Not_Raised then
      Failed ("CONSTRAINT_ERROR NOT RAISED IN " & "ACCEPT_STATEMENT - (F)");
   end if;

   if Wrong_Exc_Raised then
      Failed ("WRONG EXCEPTION RAISED IN " & "ACCEPT_STATEMENT - (F)");
   end if;

   --------------------------------------------------

   Result;
end C95008a;
