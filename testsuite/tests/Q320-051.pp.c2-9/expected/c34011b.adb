-- C34011B.ADA

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
--     CHECK THAT A DERIVED TYPE DECLARATION IS NOT CONSIDERED EXACTLY
--     EQUIVALENT TO AN ANONYMOUS DECLARATION OF THE DERIVED TYPE
--     FOLLOWED BY A SUBTYPE DECLARATION OF THE DERIVED SUBTYPE.  IN
--     PARTICULAR, CHECK THAT CONSTRAINT_ERROR CAN BE RAISED WHEN THE
--     SUBTYPE INDICATION OF THE DERIVED TYPE DECLARATION IS ELABORATED
--     (EVEN THOUGH THE CONSTRAINT WOULD SATISFY THE DERIVED (BASE)
--     TYPE).

-- HISTORY:
--     JRK 09/04/87  CREATED ORIGINAL TEST.
--     EDS 07/29/98  AVOID OPTIMIZATION

with Report; use Report;

procedure C34011b is

   subtype Bool is Boolean range False .. False;

   subtype Flt is Float range -10.0 .. 10.0;

   subtype Dur is Duration range 0.0 .. 10.0;

   subtype Int is Integer range 0 .. 10;

   type Arr is array (Int range <>) of Integer;

   type Rec (D : Int := 0) is record
      I : Integer;
   end record;

   package Pt is
      type Priv (D : Positive := 1) is private;
   private
      type Priv (D : Positive := 1) is record
         I : Integer;
      end record;
   end Pt;

   use Pt;

   type Acc_Arr is access Arr;

   type Acc_Rec is access Rec;

begin
   Test
     ("C34011B",
      "CHECK THAT CONSTRAINT_ERROR CAN BE RAISED " &
      "WHEN THE SUBTYPE INDICATION OF A DERIVED TYPE " &
      "DECLARATION IS ELABORATED");

   begin
      declare
         type T is new Bool range False .. Bool (Ident_Bool (True));

      begin
         declare
            -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
            T1 : T := T (Ident_Bool (True));
         begin
            Failed ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
         exception
            when others =>
               Failed
                 ("DID NOT RAISE CONSTRAINT_ERROR" &
                  " AT PROPER PLACE - BOOL " & T'Image (T1));   --USE T1);
         end;

         Failed ("EXCEPTION NOT RAISED - BOOL");

      exception
         when others =>
            Failed ("WRONG HANDLER ENTERED - BOOL");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - BOOL");
   end;

   begin
      declare
         type T is new Positive range Ident_Int (0) .. 10;

      begin
         declare
            -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
            T1 : T := T (Ident_Int (1));
         begin
            Failed ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
         exception
            when others =>
               Failed
                 ("DID NOT RAISE CONSTRAINT_ERROR - POSITIVE " &
                  T'Image (T1)); --USE T1
         end;
         Failed ("EXCEPTION NOT RAISED - POSITIVE");
      exception
         when others =>
            Failed ("WRONG HANDLER ENTERED - POSITIVE");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - POSITIVE");
   end;

   begin
      declare
         type T is new Flt range 0.0 .. Flt (Ident_Int (20));

      begin
         declare
            -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
            T1 : T := T (Ident_Int (0));
         begin
            Failed
              ("DID NOT RAISE CONSTRAINT_ERROR" & " AT PROPER PLACE " &
               T'Image (T1)); --USE T1

         exception
            when others =>
               Failed ("DID NOT RAISE CONSTRAINT_ERROR" & " AT PROPER PLACE ");
         end;
         Failed ("EXCEPTION NOT RAISED - FLT");
      exception
         when others =>
            Failed ("WRONG HANDLER ENTERED - FLT");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - FLT");
   end;

   begin
      declare
         type T is new Dur range Dur (Ident_Int (-1)) .. 5.0;

      begin
         declare
            -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
            T1 : T := T (Ident_Int (2));
         begin
            Failed
              ("DID NOT RAISE CONSTRAINT_ERROR" & " AT PROPER PLACE " &
               T'Image (T1));  -- USE T1
         exception
            when others =>
               Failed ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
         end;
         Failed ("EXCEPTION NOT RAISED - DUR ");
      exception
         when others =>
            Failed ("WRONG HANDLER ENTERED - DUR");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - DUR");
   end;

   begin
      declare
         type T is new Arr (Ident_Int (-1) .. 10);

      begin
         declare
            -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
            T1 : T := (others => Ident_Int (3));
         begin
            Failed
              ("DID NOT RAISE CONSTRAINT_ERROR " & "AT PROPER PLACE " &
               Integer'Image (T1 (1))); --USE T1
         exception
            when others =>
               Failed ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
         end;
         Failed ("EXCEPTION NOT RAISED - ARR ");
      exception
         when others =>
            Failed ("WRONG HANDLER ENTERED - ARR");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - ARR");
   end;

   begin
      declare
         type T is new Rec (Ident_Int (11));

      begin
         declare
            -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
            T1 : T;
         begin
            Failed ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
         exception
            when others =>
               Failed
                 ("DID NOT RAISE CONSTRAINT_ERROR " & "AT PROPER PLACE " &
                  Integer'Image (T1.D)); --USE T1
         end;
         Failed ("EXCEPTION NOT RAISED - REC ");
      exception
         when others =>
            Failed ("WRONG HANDLER ENTERED - REC");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - REC");
   end;

   begin
      declare
         type T is new Priv (Ident_Int (0));  --RAISES C_E

      begin
         declare
            -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
            T1 : T;
         begin
            Failed ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
         exception
            when others =>
               Failed
                 ("DID NOT RAISE CONSTRAINT_ERROR " & "AT PROPER PLACE " &
                  Integer'Image (T1.D)); --USE T1
         end;
         Failed ("EXCEPTION NOT RAISED - PRIV ");
      exception
         when others =>
            Failed ("WRONG HANDLER ENTERED - PRIV");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - PRIV");
   end;

   begin
      declare
         type T is new Acc_Arr (0 .. Ident_Int (11));  --RAISES C_E

      begin
         declare
            -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
            T1 : T;
         begin
            Failed ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
         exception
            when others =>
               Failed
                 ("DID NOT RAISE CONSTRAINT_ERROR " & "AT PROPER PLACE " &
                  Integer'Image (T1 (1))); --USE T1
         end;
         Failed ("EXCEPTION NOT RAISED - ACC_ARR ");
      exception
         when others =>
            Failed ("WRONG HANDLER ENTERED - ACC_ARR");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - ACC_ARR");
   end;

   begin
      declare
         type T is new Acc_Rec (Ident_Int (-1));  --RAISES C_E

      begin
         declare
            -- DEFINE AN OBJECT OF TYPE T AND USE IT TO AVOID OPTIMIZATION
            T1 : T;
         begin
            Failed ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
         exception
            when others =>
               Failed
                 ("DID NOT RAISE CONSTRAINT_ERROR " & "AT PROPER PLACE " &
                  Integer'Image (T1.D)); --USE T1
         end;
         Failed ("EXCEPTION NOT RAISED - ACC_REC ");
      exception
         when others =>
            Failed ("WRONG HANDLER ENTERED - ACC_REC");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - ACC_REC");
   end;

   Result;
end C34011b;
