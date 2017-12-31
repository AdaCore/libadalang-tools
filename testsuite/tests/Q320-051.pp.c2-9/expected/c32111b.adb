-- C32111B.ADA

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
-- CHECK THAT WHEN A VARIABLE OR CONSTANT HAVING AN ENUMERATION,
--    INTEGER, FLOAT OR FIXED TYPE IS DECLARED WITH AN INITIAL STATIC
--    VALUE, CONSTRAINT_ERROR IS RAISED IF THE INITIAL VALUE LIES
--    OUTSIDE THE RANGE OF THE SUBTYPE.

-- HISTORY:
--    JET 08/04/87  CREATED ORIGINAL TEST BASED ON C32111A BY RJW
--                  BUT WITH STATIC VALUES INSTEAD OF DYNAMIC
--                  IDENTITY FUNCTION.

with Report; use Report;

procedure C32111b is

   type Weekday is (Mon, Tues, Wed, Thurs, Fri);
   subtype Midweek is Weekday range Wed .. Wed;

   subtype Digit is Character range '0' .. '9';

   subtype Short is Integer range -100 .. 100;

   type Int is range -10 .. 10;
   subtype Pint is Int range 1 .. 10;

   type Flt is digits 3 range -5.0 .. 5.0;
   subtype Sflt is Flt range -5.0 .. 0.0;

   type Fixed is delta 0.5 range -5.0 .. 5.0;
   subtype Sfixed is Fixed range 0.0 .. 5.0;

begin
   Test
     ("C32111B",
      "CHECK THAT WHEN A VARIABLE OR CONSTANT " &
      "HAVING AN ENUMERATION, INTEGER, FLOAT OR " &
      "FIXED TYPE IS DECLARED WITH AN INITIAL STATIC " &
      "VALUE, CONSTRAINT_ERROR IS RAISED IF THE " &
      "INITIAL VALUE LIES OUTSIDE THE RANGE OF THE " & "SUBTYPE");

   begin
      declare
         D : Midweek := Weekday'Val (1);
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'D'");
         if D = Tues then
            Comment ("VARIABLE 'D' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'D'");
   end;

   begin
      declare
         D : constant Weekday range Wed .. Wed := Weekday'Val (3);
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'D'");
         if D = Tues then
            Comment ("INITIALIZE VARIABLE 'D'");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'D'");
   end;

   begin
      declare
         P : constant Digit := '/';
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'P'");
         if P = '0' then
            Comment ("VARIABLE 'P' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'P'");
   end;

   begin
      declare
         Q : Character range 'A' .. 'E' := 'F';
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'Q'");
         if Q = 'A' then
            Comment ("VARIABLE 'Q' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'Q'");
   end;

   begin
      declare
         I : Short := -101;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'I'");
         if I = 1 then
            Comment ("VARIABLE 'I' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'I'");
   end;

   begin
      declare
         J : constant Integer range 0 .. 100 := 101;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'J'");
         if J = -1 then
            Comment ("VARIABLE 'J' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'J'");
   end;

   begin
      declare
         K : Int range 0 .. 1 := 2;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'K'");
         if K = 2 then
            Comment ("VARIABLE 'K' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'K'");
   end;

   begin
      declare
         L : constant Pint := 0;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'L'");
         if L = 1 then
            Comment ("VARIABLE 'L' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'L'");
   end;

   begin
      declare
         Fl : Sflt := 1.0;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'FL'");
         if Fl = 3.14 then
            Comment ("VARIABLE 'FL' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'FL'");
   end;

   begin
      declare
         Fl1 : constant Flt range 0.0 .. 0.0 := -1.0;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'FL1'");
         if Fl1 = 0.0 then
            Comment ("VARIABLE 'FL1' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'FL1'");
   end;

   begin
      declare
         Fi : Fixed range 0.0 .. 0.0 := 0.5;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'FI'");
         if Fi = 0.5 then
            Comment ("VARIABLE 'FI' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'FI'");
   end;

   begin
      declare
         Fi1 : constant Sfixed := -0.5;
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'FI1'");
         if Fi1 = 0.5 then
            Comment ("VARIABLE 'FI1' INITIALIZED");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'FI1'");
   end;

   Result;
end C32111b;
