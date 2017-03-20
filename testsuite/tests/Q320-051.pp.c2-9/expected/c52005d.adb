-- C52005D.ADA

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
-- CHECK THAT THE CONSTRAINT_ERROR EXCEPTION IS RAISED WHEN A DYNAMIC
--    EXPRESSION VALUE IS OUTSIDE THE STATIC RANGE OF INTEGER, BOOLEAN,
--    CHARACTER, AND ENUMERATION ASSIGNMENT TARGET VARIABLES.

-- JRK 7/21/80
-- SPS 3/21/83

with Report;
procedure C52005d is

   use Report;

begin
   Test
     ("C52005D",
      "CHECK THAT CONSTRAINT_ERROR EXCEPTION IS RAISED " &
      "ON DYNAMIC OUT OF RANGE INTEGER, BOOLEAN, CHARACTER, " &
      "AND ENUMERATION ASSIGNMENTS");

-------------------------

   declare
      I1 : Integer range 0 .. 10 := 5;

   begin
      I1 := Ident_Int (11);

      Failed ("EXCEPTION NOT RAISED FOR OUT OF RANGE INT ASSNMT");

   exception
      when Constraint_Error =>
         if I1 /= 5 then
            Failed ("VALUE ALTERED BEFORE INT RANGE EXCEPTION");
         end if;

   end;

-------------------------

   declare
      I2 : Integer range 0 .. 10 := 5;

   begin
      I2 := Ident_Int (10);

   exception
      when Constraint_Error =>
         Failed ("EXCEPTION RAISED ON LEGAL INTEGER ASSIGNMENT");
   end;

-------------------------

   declare
      B1 : Boolean range True .. True := True;

   begin
      B1 := Ident_Bool (False);

      Failed ("EXCEPTION NOT RAISED FOR OUT OF RANGE BOOL ASSNMT");

   exception
      when Constraint_Error =>
         if B1 /= True then
            Failed ("VALUE ALTERED BEFORE BOOLEAN RANGE EXCEPTION");
         end if;
   end;

-------------------------

   declare
      B2 : Boolean := True;

   begin
      B2 := Ident_Bool (False);

   exception
      when Constraint_Error =>
         Failed ("EXCEPTION RAISED ON LEGAL BOOLEAN ASSNMNT");

   end;

-------------------------

   declare
      C1 : Character range 'B' .. 'Z' := 'M';

   begin
      C1 := Ident_Char ('A');
      Failed ("EXCEPTION NOT RAISED FOR OUT OF RANGE CHAR ASSNMNT");

   exception
      when Constraint_Error =>
         if C1 /= 'M' then
            Failed ("VALUE ALTERED BEFORE CHARACTER RANGE " & "EXCEPTION");
         end if;

   end;

-------------------------

   declare
      C2 : Character range 'B' .. 'Z' := 'M';

   begin
      C2 := Ident_Char ('B');

   exception
      when Constraint_Error =>
         Failed ("EXCEPTION RAISED OF LEGAL CHARACTER ASSNMNT");

   end;

-------------------------

   declare
      type Day is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      Alldays : Day                  := Tue;
      Workday : Day range Mon .. Fri := Tue;

   begin
      if Equal (3, 3) then
         Alldays := Sun;
      end if;
      Workday := Alldays;

      Failed ("EXCEPTION NOT RAISED FOR OUT OF RANGE ENUM. " & "ASSIGNMENT");

   exception
      when Constraint_Error =>
         if Workday /= Tue then
            Failed ("VALUE ALTERED BEFORE ENUM. RANGE EXCEPTION");
         end if;

   end;

-------------------------

   declare
      type Day is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
      Alldays : Day                  := Tue;
      Workday : Day range Mon .. Fri := Tue;

   begin
      if Equal (3, 3) then
         Alldays := Fri;
      end if;
      Workday := Alldays;

   exception
      when Constraint_Error =>
         Failed ("EXCEPTION RAISED ON LEGAL ENUM. ASSNMNT");

   end;

-------------------------

   Result;
end C52005d;
