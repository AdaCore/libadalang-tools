-- C36104A.ADA

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
-- EXCEPTIONS ARE NOT RAISED FOR NULL RANGES. ONLY STATIC CASES ARE CHECKED
-- HERE.

-- DAT 2/3/81
-- JRK 2/25/81
-- VKG 1/21/83
-- L.BROWN 7/15/86 1) ADDED ACCESS TYPES.
--                   2) DELETED "NULL INDEX RANGES, CONSTRAINT_ERROR
--                      RAISED" SECTION.
--                   3) DELETED ANY MENTION OF CASE STATEMENT CHOICES
--                      AND VARIANT CHOICES IN THE ABOVE COMMENT.
-- EDS 7/16/98 AVOID OPTIMIZATION

with Report;
procedure C36104a is

   use Report;

   type Week is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   type Week_Array is array (Week range <>) of Week;
   subtype Work_Week is Week range Mon .. Fri;
   subtype Mid_Week is Work_Week range Tue .. Thu;

   type Int_10 is new Integer range -10 .. 10;
   type I_10 is new Int_10;
   subtype I_5 is I_10 range -5 .. 5;
   type I_5_Array is array (I_5 range <>) of I_5;

begin
   Test
     ("C36104A",
      "CONSTRAINT_ERROR IS RAISED OR NOT IN STATIC " &
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
            A1 : A := (others => I_5 (Ident_Int (1)));
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
         Failed ("CONSTRAINT_ERROR NOT RAISED 3");
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
         type P is access I_5_Array (I_5 range 0 .. 6);
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
      Failed
        ("CONSTRAINT_ERROR NOT RAISED 7 " & Mid_Week'Image (W (Wed))); --USE W
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
      Failed
        ("CONSTRAINT_ERROR NOT RAISED 8 " & Mid_Week'Image (W (Wed))); --USE W
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
         W := (W'Range => Wed); -- OK.
         Failed
           ("CONSTRAINT_ERROR NOT RAISED 9 " &
            Mid_Week'Image (W (Wed))); --USE W
      exception
         when others =>
            Failed ("UNHANDLED EXCEPTION RAISED 9");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 9");
   end;

   begin
      declare
         type W is new Week_Array (Mid_Week range Sun .. Tue);
      -- RAISES CONSTRAINT_ERROR.
      begin
         declare
            W1 : W := (others => Wed);
         begin
            Failed
              ("CONSTRAINT_ERROR NOT RAISED 10 " &
               Mid_Week'Image (W1 (Wed))); --USE W1
         end;
      exception
         when others =>
            Failed ("UNHANDLED EXCEPTION RAISED 10");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED 10");
   end;

   begin
      declare
         subtype W is Week_Array (Mid_Week range Mon .. Wed);
      -- RAISES CONSTRAINT_ERROR.
      begin
         declare
            W1 : W := (others => (Wed));
         begin
            Failed
              ("CONSTRAINT_ERROR NOT RAISED 8 " &
               Mid_Week'Image (W1 (Wed))); --USE W1
         end;
      exception
         when others =>
            Failed ("UNHANDLED EXCEPTION RAISED 8");
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
         type A is array (I_5 range -5 .. -6) of I_5;
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
         Failed ("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
      end loop;
      for I in Mid_Week range Fri .. Wed loop
         Failed ("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
      end loop;
      for I in Mid_Week range Mon .. Sun loop
         Failed ("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
      end loop;
      for I in I_5 range 10 .. -10 loop
         Failed ("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
      end loop;
      for I in I_5 range 10 .. 9 loop
         Failed ("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
      end loop;
      for I in I_5 range -10 .. -11 loop
         Failed ("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
      end loop;
      for I in I_5 range -10 .. -20 loop
         Failed ("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
      end loop;
      for I in I_5 range 6 .. 5 loop
         Failed ("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
      end loop;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 3");
   end;

   begin
      declare
         type P is access I_5_Array (-5 .. -6);
         Pa1 : P := new I_5_Array (-5 .. -6);
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
         if (W'First /= Mon) then
            Failed ("'FIRST OF NULL ARRAY INCORRECT");
         end if;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 9");
   end;

   begin
      declare
         type W is new Week_Array (Mid_Week range Tue .. Mon);
         W1 : W;
      begin
         if (W1'First /= Tue) then
            Failed ("'FIRST OF NULL ARRAY INCORRECT");
         end if;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 10");
   end;

   begin
      declare
         subtype W is Week_Array (Mid_Week range Tue .. Mon);
         W1 : W;
      begin
         if (W1'First /= Tue) then
            Failed ("'FIRST OF NULL ARRAY INCORRECT");
         end if;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 12");
   end;

   -- NULL MEMBERSHIP RANGES, EXCEPTION NOT RAISED.

   begin
      if Sun in Sat .. Sun or
        Sat in Fri .. Wed or
        Wed in Thu .. Tue or
        Thu in Mon .. Sun or
        Fri in Sat .. Fri or
        Wed in Fri .. Mon
      then
         Failed ("INCORRECT 'IN' EVALUATION 1");
      end if;

      if Integer'(0) in 10 .. -10 or
        Integer'(0) in 10 .. 9 or
        Integer'(0) in -10 .. -11 or
        Integer'(0) in -10 .. -20 or
        Integer'(0) in 6 .. 5 or
        Integer'(0) in 5 .. 3 or
        Integer'(0) in 7 .. 3
      then
         Failed ("INCORRECT 'IN' EVALUATION 2");
      end if;

      if Wed not in Thu .. Tue and Integer'(0) not in 4 .. -4 then
         null;
      else
         Failed ("INCORRECT 'NOT IN' EVALUATION");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED 52");
   end;

   Result;
end C36104a;
