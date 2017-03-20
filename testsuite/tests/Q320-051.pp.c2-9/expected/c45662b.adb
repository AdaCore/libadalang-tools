-- C45662B.ADA

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
-- CHECK THE TRUTH TABLE FOR  'NOT'  ON DERIVED-BOOLEAN-TYPE OPERANDS.

-- THE COMBINATIONS OF  'NOT'  WITH  'AND' , 'OR' , 'XOR'  ARE TESTED
--    IN C45101K.

-- RM    28 OCTOBER 1980
-- TBN 10/21/85     RENAMED FROM C45401B-AB.ADA.  REMOVED DUPLICATED
--                  CODE NEAR END.

with Report; use Report;
procedure C45662b is

   type Nb is new Boolean;

   Tvar, Fvar, Cvar : Nb      := Nb'(False); -- INITIAL VALUE IRRELEVANT
   Error_Count      : Integer := 0;            -- INITIAL VALUE ESSENTIAL

   procedure Bump is
   begin
      Error_Count := Error_Count + 1;
   end Bump;

   function Ident_New_Bool (The_Argument : Nb) return Nb is
   begin
      if Equal (2, 2) then
         return The_Argument;
      else
         return Nb'(False);
      end if;
   end Ident_New_Bool;

begin

   Test
     ("C45662B",
      "CHECK THE TRUTH TABLE FOR  'NOT'" &
      " ON DERIVED-BOOLEAN-TYPE OPERANDS");

   for A in Nb loop

      Cvar := not A;

      if Boolean (not A) then
         if Boolean (A) then
            Bump;
         end if;
      end if;

      if Boolean (Cvar) then
         if Boolean (A) then
            Bump;
         end if;
      end if;

      if Boolean
          (
not
           (not
            (not
             (not
              (not
               (not
                (not
                 (not
                  (not
                   (not
                    (not
                     (not
                      (not
                       (not
                        (not
                         (not (not (not (not (not (Cvar)))))))))))))))))))))
      then
         if Boolean (A) then
            Bump;
         end if;
      end if;

   end loop;

   for I in 1 .. 2 loop

      Cvar := not (Nb (I > 1));

      if Boolean (not (Nb (I > 1))) then
         if I > 1 then
            Bump;
         end if;
      end if;

      if Boolean (Cvar) then
         if I > 1 then
            Bump;
         end if;
      end if;

   end loop;

   if Boolean (not (Nb'(True))) then
      Bump;
   end if;
   if Boolean (not (Nb'(False))) then
      null;
   else
      Bump;
   end if;

   Tvar := Ident_New_Bool (Nb'(True));
   Fvar := Ident_New_Bool (Nb'(False));

   if Boolean (not Tvar) then
      Bump;
   end if;
   if Boolean (not Fvar) then
      null;
   else
      Bump;
   end if;

   if Error_Count /= 0 then
      Failed ("'NOT' TRUTH TABLE");
   end if;

   Result;

end C45662b;
