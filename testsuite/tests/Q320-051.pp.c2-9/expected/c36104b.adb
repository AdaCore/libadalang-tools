-- C36104B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED OR NOT, AS APPROPRIATE, DURING
-- DISCRETE_RANGE ELABORATIONS/EVALUATIONS IN LOOPS, ARRAY_TYPE_DEFINITIONS,
-- ARRAY AGGREGATES, SLICES, AND INDEX CONSTRAINTS IN OBJECT AND
-- TYPE DECLARATIONS, WHERE AN EXPLICIT (SUB)TYPE IS INCLUDED IN EACH
-- DISCRETE_RANGE. MEMBERSHIP OPERATORS ARE CHECKED HERE, ALSO, TO ENSURE THAT
-- EXCEPTIONS ARE NOT RAISED FOR NULL RANGES. ONLY DYNAMIC CASES ARE CHECKED
-- HERE.

-- DAT 2/3/81
-- JRK 2/25/81
-- L.BROWN 7/15/86 1) ADDED ACCESS TYPES.
--                   2) DELETED "NULL INDEX RANGE, CONSTRAINT_ERROR
--                      RAISED" SECTION.
--                   3) MADE USE OF DYNAMIC-RESULT FUNCTIONS.
--                   4) DELETED ALL REFERENCES TO CASE STATEMENT CHOICES
--                      AND VARIANT PART CHOICES IN THE ABOVE COMMENT.
-- EDS 7/16/98 AVOID OPTIMIZATION

with Report;
procedure C36104b is

   use Report;

   type Week is (Ssun, Smon, Stue, Swed, Sthu, Sfri, Ssat);
   Sun : Week := Week'Val (Ident_Int (0));
   Mon : Week := Week'Val (Ident_Int (1));
   Tue : Week := Week'Val (Ident_Int (2));
   Wed : Week := Week'Val (Ident_Int (3));
   Thu : Week := Week'Val (Ident_Int (4));
   Fri : Week := Week'Val (Ident_Int (5));
   Sat : Week := Week'Val (Ident_Int (6));
   type Week_Array is array (Week range <>) of Week;
   subtype Work_Week is Week range Mon .. Fri;
   subtype Mid_Week is Work_Week range Tue .. Thu;

   type Int_10 is new Integer range -10 .. 10;
   type I_10 is new Int_10;
   subtype I_5 is I_10 range I_10 (Ident_Int (-5)) .. I_10 (Ident_Int (5));
   type I_5_Array is array (I_5 range <>) of I_5;

   function F (Day : Week) return Week is
   begin
      return Day;
   end F;

begin
   Test
     ("C36104B",
      "CONSTRAINT_ERROR IS RAISED OR NOT IN DYNAMIC " &
      "DISCRETE_RANGES WITH EXPLICIT TYPE_MARKS");

   -- NON-NULL RANGES, CONSTRAINT_ERROR RAISED.

   begin
      declare
         type A is array (I_5 range 0 .. 6) of I_5;
         -- ABOVE DECLARATION RAISES CONSTRAINT_ERROR.
      begin
         declare
            -- DEFINE AN OBJECT OF TYPE A AND USE IT TO AVOID OPTIMIZATION OF
            -- SUBTYPE
            A1 : A := (A'Range => I_5 (Ident_Int (1)));
         begin
            Failed
              ("CONSTRAINT_ERROR NOT RAISED 1 " &
               I_5'Image (A1 (1)));  --USE A1
         end;
      exception
         --MAKE SURE THAT CONSTRAINT_ERROR FROM ABOVE STATEMENTS
         --REPORT FAILED.
         when others =>
            Failed ("UNHANDLED EXCEPTION RAISED 1");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 1");
   end;

   begin
      for I in Mid_Week range Mon .. Mon loop

         if Equal (2, 2) then
            Sat := Ssat;
         end if;

      end loop;
      Failed ("CONSTRAINT_ERROR NOT RAISED 3");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 3");
   end;

   begin
      declare
         type P is access I_5_Array (0 .. 6);
         -- ABOVE DECLARATION RAISES CONSTRAINT_ERROR.
      begin
         declare
            type Pa is new P;
            -- DEFINE AN OBJECT OF TYPE PA AND USE IT TO AVOID OPTIMIZATION OF
            -- TYPE
            Pa1 : Pa :=
              new I_5_Array'(0 .. I_5 (Ident_Int (6)) => I_5 (Ident_Int (1)));
         begin
            Failed
              ("CONSTRAINT_ERROR NOT RAISED 4 " &
               I_5'Image (Pa1 (1))); --USE PA1
         end;
      exception
         when others =>
            Failed ("UNHANDLED EXCEPTION RAISED 4");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 4");
   end;

   declare
      W : Week_Array (Mid_Week);
   begin
      W := (Mid_Week range Mon .. Wed => Wed);
      -- CONSTRAINT_ERROR RAISED.
      begin
         Failed
           ("CONSTRAINT_ERROR NOT RAISED 7 " &
            Mid_Week'Image (W (Wed))); --USE W
      exception
         when others =>
            Failed ("UNHANDLED EXCEPTION RAISED 7");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 7");
   end;

   declare
      W : Week_Array (Work_Week);
   begin
      W              := (W'Range => Wed); -- OK.
      W (Mon .. Wed) := W (Mid_Week range Mon .. Wed); -- EXCEPTION.
      begin
         Failed
           ("CONSTRAINT_ERROR NOT RAISED 8 " &
            Mid_Week'Image (W (Wed))); --USE W
      exception
         when others =>
            Failed ("UNHANDLED EXCEPTION RAISED 8");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 8");
   end;

   begin
      declare
         W : Week_Array (Mid_Week range Mon .. Fri);
         -- ELABORATION OF ABOVE RAISES CONSTRAINT_ERROR.
      begin
         W (Wed) := Thu;        -- OK.
         Failed
           ("CONSTRAINT_ERROR NOT RAISED 9 " &
            Week'Image (W (Wed)));   -- USE W
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 9");
   end;

   begin
      declare
         type W is new Week_Array (Mid_Week range Sun .. Wed);
         -- RAISES CONSTRAINT_ERROR.
      begin
         declare
            X : W;              -- OK.
         begin
            X (Tue) := Thu;   -- OK.
            Failed
              ("CONSTRAINT_ERROR NOT RAISED 10 " &
               Week'Image (X (Tue)));   -- USE X
         end;
      exception
         when others =>
            Failed ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 10");
   end;

   begin
      declare
         subtype W is Week_Array (Mid_Week range Mon .. Thu);
         -- RAISES CONSTRAINT_ERROR.
      begin
         declare
            T : W;               -- OK.
         begin
            T (Tue) := Thu;    -- OK.
            Failed ("CONSTRAINT_ERROR NOT RAISED 11 " & Week'Image (T (Tue)));
         end;
      exception
         when others =>
            Failed ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 11");
   end;

   -- NULL DISCRETE/INDEX RANGES, EXCEPTION NOT RAISED.

   begin
      declare
         type A is array (I_5 range I_5 (Ident_Int (-5)) .. -6) of I_5;
         A1 : A;
      begin
         if A1'First /= I_5 (Ident_Int (-5)) then
            Failed ("'FIRST OF NULL ARRAY INCORRECT");
         end if;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 1");
   end;

   begin
      for I in Mid_Week range Sat .. Sun loop

         if Equal (2, 2) then
            Tue := Stue;
         end if;

      end loop;
      for I in Mid_Week range Fri .. Wed loop

         if Equal (2, 2) then
            Mon := Smon;
         end if;

      end loop;
      for I in Mid_Week range Mon .. Sun loop

         if Equal (3, 3) then
            Wed := Swed;
         end if;

      end loop;
      for I in I_5 range 10 .. -10 loop

         if Equal (2, 2) then
            Tue := Stue;
         end if;

      end loop;
      for I in I_5 range 10 .. 9 loop

         if Equal (2, 2) then
            Thu := Sthu;
         end if;

      end loop;
      for I in I_5 range -10 .. -11 loop

         if Equal (2, 2) then
            Sat := Ssat;
         end if;

      end loop;
      for I in I_5 range -10 .. -20 loop

         if Equal (2, 2) then
            Sun := Ssun;
         end if;

      end loop;
      for I in I_5 range 6 .. 5 loop

         if Equal (2, 2) then
            Mon := Smon;
         end if;

      end loop;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 3");
   end;

   begin
      declare
         type P is access I_5_Array (I_5 (Ident_Int (-5)) .. -6);
         Pa1 : P := new I_5_Array (I_5 (Ident_Int (-5)) .. -6);
      begin
         if Pa1'Length /= Ident_Int (0) then
            Failed ("'LENGTH OF NULL ARRAY INCORRECT");
         end if;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 5");
   end;

   declare
      type Narr is array (Integer range <>) of Integer;
      subtype Snarr is Integer range 1 .. 2;
      W : Narr (Snarr) := (1, 2);
   begin
      if W = (Snarr range Ident_Int (4) .. 2 => 5) then
         Failed ("EVALUATION OF EXPRESSION IS INCORRECT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 7");
   end;

   declare
      W : Week_Array (Mid_Week);
   begin
      W              := (W'Range => Wed); -- OK.
      W (Tue .. Mon) := W (Mid_Week range Mon .. Sun);
   exception
      when others =>
         Failed ("EXCEPTION RAISED 8");
   end;

   begin
      declare
         W : Week_Array (Mid_Week range Mon .. Sun);
      begin

         if Equal (W'Length, 0) then
            Tue := Stue;
         end if;

      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 9");
   end;

   begin
      declare
         type W is new Week_Array (Mid_Week range Tue .. Mon);
      begin

         if Equal (W'Length, 0) then
            Mon := Smon;
         end if;

      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 10");
   end;

   begin
      declare
         subtype W is Week_Array (Mid_Week range Tue .. Mon);
      begin

         if Equal (W'Length, 0) then
            Wed := Swed;
         end if;

      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 12");
   end;

   -- NULL MEMBERSHIP RANGES, EXCEPTION NOT RAISED.

   begin
      if F (Sun) in Sat .. Sun or Sat in Fri .. Wed or F (Wed) in Thu .. Tue or
        Thu in Mon .. Sun or F (Fri) in Sat .. Fri or Wed in Fri .. Mon then
         Failed ("INCORRECT 'IN' EVALUATION 1");
      end if;

      if Ident_Int (0) in 10 .. Ident_Int (-10) or 0 in Ident_Int (10) .. 9 or
        Ident_Int (0) in Ident_Int (-10) .. -11 or
        0 in -10 .. Ident_Int (-20) or Ident_Int (0) in 6 .. Ident_Int (5) or
        0 in 5 .. Ident_Int (3) or Ident_Int (0) in 7 .. Ident_Int (3) then
         Failed ("INCORRECT 'IN' EVALUATION 2");
      end if;

      if F (Wed) not in Thu .. Tue and Ident_Int (0) not in Ident_Int (4) .. -4
      then
         null;
      else
         Failed ("INCORRECT 'NOT IN' EVALUATION");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 52");
   end;

   Result;
end C36104b;
