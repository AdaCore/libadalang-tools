-- C47002D.ADA

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
-- CHECK THAT VALUES BELONGING TO EACH CLASS OF TYPE CAN BE WRITTEN AS
-- THE OPERANDS OF QUALIFIED EXPRESSIONS.
-- THIS TEST IS FOR PRIVATE AND LIMITED PRIVATE TYPES.

-- RJW 7/23/86

with Report; use Report;
procedure C47002d is

begin

   Test
     ("C47002D",
      "CHECK THAT VALUES HAVING PRIVATE AND LIMITED " &
      "PRIVATE TYPES CAN BE WRITTEN AS THE OPERANDS " &
      "OF QUALIFIED EXPRESSIONS");

   declare -- PRIVATE TYPES.

      type Results is (P1, P2, P3, P4, P5);

      package Pkg1 is
         type Pint is private;
         type Pchar is private;
         type Parr is private;
         type Prec (D : Integer) is private;
         type Pacc is private;

         function F return Pint;
         function F return Pchar;
         function F return Parr;
         function F return Prec;
         function F return Pacc;

      private
         type Pint is new Integer;
         type Pchar is new Character;
         type Parr is array (1 .. 2) of Natural;

         type Prec (D : Integer) is record
            null;
         end record;

         type Pacc is access Prec;

      end Pkg1;

      package body Pkg1 is
         function F return Pint is
         begin
            return 1;
         end F;

         function F return Pchar is
         begin
            return 'B';
         end F;

         function F return Parr is
         begin
            return Parr'(others => 3);
         end F;

         function F return Prec is
         begin
            return Prec'(D => 4);
         end F;

         function F return Pacc is
         begin
            return new Prec'(F);
         end F;

      end Pkg1;

      package Pkg2 is
      end Pkg2;

      package body Pkg2 is
         use Pkg1;

         function Check (P : Pint) return Results is
         begin
            return P1;
         end Check;

         function Check (P : Pchar) return Results is
         begin
            return P2;
         end Check;

         function Check (P : Parr) return Results is
         begin
            return P3;
         end Check;

         function Check (P : Prec) return Results is
         begin
            return P4;
         end Check;

         function Check (P : Pacc) return Results is
         begin
            return P5;
         end Check;

      begin
         if Check (Pint'(F)) /= P1 then
            Failed ("INCORRECT RESULTS FOR TYPE PINT");
         end if;

         if Check (Pchar'(F)) /= P2 then
            Failed ("INCORRECT RESULTS FOR TYPE PCHAR");
         end if;

         if Check (Parr'(F)) /= P3 then
            Failed ("INCORRECT RESULTS FOR TYPE PARR");
         end if;

         if Check (Prec'(F)) /= P4 then
            Failed ("INCORRECT RESULTS FOR TYPE PREC");
         end if;

         if Check (Pacc'(F)) /= P5 then
            Failed ("INCORRECT RESULTS FOR TYPE PACC");
         end if;

      end Pkg2;

   begin
      null;
   end;

   declare -- LIMITED PRIVATE TYPES.

      type Results is (Lp1, Lp2, Lp3, Lp4, Lp5);

      package Pkg1 is
         type Lpint is limited private;
         type Lpchar is limited private;
         type Lparr is limited private;
         type Lprec (D : Integer) is limited private;
         type Lpacc is limited private;

         function F return Lpint;
         function F return Lpchar;
         function F return Lparr;
         function F return Lprec;
         function F return Lpacc;

      private
         type Lpint is new Integer;
         type Lpchar is new Character;
         type Lparr is array (1 .. 2) of Natural;

         type Lprec (D : Integer) is record
            null;
         end record;

         type Lpacc is access Lprec;

      end Pkg1;

      package body Pkg1 is
         function F return Lpint is
         begin
            return 1;
         end F;

         function F return Lpchar is
         begin
            return 'B';
         end F;

         function F return Lparr is
         begin
            return Lparr'(others => 3);
         end F;

         function F return Lprec is
         begin
            return Lprec'(D => 4);
         end F;

         function F return Lpacc is
         begin
            return new Lprec'(F);
         end F;

      end Pkg1;

      package Pkg2 is
      end Pkg2;

      package body Pkg2 is
         use Pkg1;

         function Check (Lp : Lpint) return Results is
         begin
            return Lp1;
         end Check;

         function Check (Lp : Lpchar) return Results is
         begin
            return Lp2;
         end Check;

         function Check (Lp : Lparr) return Results is
         begin
            return Lp3;
         end Check;

         function Check (Lp : Lprec) return Results is
         begin
            return Lp4;
         end Check;

         function Check (Lp : Lpacc) return Results is
         begin
            return Lp5;
         end Check;

      begin
         if Check (Lpint'(F)) /= Lp1 then
            Failed ("INCORRECT RESULTS FOR TYPE LPINT");
         end if;

         if Check (Lpchar'(F)) /= Lp2 then
            Failed ("INCORRECT RESULTS FOR TYPE LPCHAR");
         end if;

         if Check (Lparr'(F)) /= Lp3 then
            Failed ("INCORRECT RESULTS FOR TYPE LPARR");
         end if;

         if Check (Lprec'(F)) /= Lp4 then
            Failed ("INCORRECT RESULTS FOR TYPE LPREC");
         end if;

         if Check (Lpacc'(F)) /= Lp5 then
            Failed ("INCORRECT RESULTS FOR TYPE LPACC");
         end if;

      end Pkg2;

   begin
      null;
   end;

   Result;
end C47002d;
