-- CC3012A.ADA

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
-- CHECK THAT GENERIC INSTANCES MAY BE OVERLOADED.

-- CHECK THAT THEY MAY OVERLOAD PREVIOUSLY DECLARED SUBPROGRAMS AND ENUMERATION
-- LITERALS.

-- DAT 9/16/81
-- SPS 10/19/82
-- SPS 2/8/83
-- PWN 11/30/94 REMOVED PART OF TEST INVALID FOR ADA 9X.

with Report; use Report;

procedure Cc3012a is
begin
   Test
     ("CC3012A",
      "CHECK THAT GENERIC INSTANCES MAY OVERLOAD " & "OTHER IDENTIFIERS");

   declare
      generic
         type T is (<>);
         V : in T;
      procedure Gp (X : in out T);

      generic
         type T is (<>);
      function Less (X, Y : T) return Boolean;

      generic
         type T is (<>);
      function Plus (X, Y : T) return T;

      generic
         type T is private;
         Z : T;
      function F1 return T;

      type Dc is new Character range Ident_Char ('A') .. 'Z';
      type Di is new Integer;
      type Enum is (E1, E2, E3, E4);

      Vc : Character := 'A';
      Vi : Integer   := 5;
      Vb : Boolean   := True;
      Ve : Enum      := E2;

      type Denum is new Enum range E2 .. Enum'Last;

      Vde : Denum := E4;
      Vdc : Dc    := 'A';
      Vdi : Di    := 7;

      procedure Gp (X : in out T) is
      begin
         X := V;
      end Gp;

      function Less (X, Y : T) return Boolean is
      begin
         return False;
      end Less;

      function Plus (X, Y : T) return T is
      begin
         return T'First;
      end Plus;

      function F1 return T is
      begin
         return Z;
      end F1;

      function E5 return Integer is
      begin
         return 1;
      end E5;

      package Pkg is

         procedure P is new Gp (Character, 'Q');
         procedure P is new Gp (Integer, -14);
         procedure P is new Gp (Boolean, False);
         procedure P is new Gp (Enum, E4);
         procedure P is new Gp (Dc, 'W');
         procedure P is new Gp (Di, -33);
         procedure P is new Gp (Denum, E2);

         function "<" is new Less (Character);
         function "<" is new Less (Integer);
         function "<" is new Less (Boolean);
         function "<" is new Less (Enum);
         function "<" is new Less (Dc);
         function "<" is new Less (Di);
         -- NOT FOR DENUM.

         function "+" is new Plus (Character);
         function "+" is new Plus (Integer);
         function "+" is new Plus (Boolean);
         function "+" is new Plus (Enum);
         function "+" is new Plus (Dc);
         -- NOT FOR DI.
         function "+" is new Plus (Denum);

         function E2 is new F1 (Boolean, False);
         function E5 is new F1 (Dc, 'M');

      end Pkg;

      package body Pkg is
      begin
         P (Vc);
         P (Vi);
         P (Vb);
         P (Ve);
         P (X => Vde);
         P (X => Vdc);
         P (X => Vdi);

         if Vc /= 'Q' then
            Failed ("OVERLOADED PROCEDURE - 1");
         end if;

         if Vi /= -14 then
            Failed ("OVERLOADED PROCEDURE - 2");
         end if;

         if Vb /= False then
            Failed ("OVERLOADED PROCEDURE - 3");
         end if;

         if Ve /= E4 then
            Failed ("OVERLOADED PROCEDURE - 4");
         end if;

         if Vde /= E2 then
            Failed ("OVERLOADED PROCEDURE - 5");
         end if;

         if Vdc /= 'W' then
            Failed ("OVERLOADED PROCEDURE - 6");
         end if;

         if Vdi /= -33 then
            Failed ("OVERLOADED PROCEDURE - 7");
         end if;

         if Vc < Ascii.Del then
            Failed ("OVERLOADED LESS THAN - 1");
         end if;

         if Vi < 1E3 then
            Failed ("OVERLOADED LESS THAN - 2");
         end if;

         if False < True then
            Failed ("OVERLOADED LESS THAN - 3");
         end if;

         if E1 < Ve then
            Failed ("OVERLOADED LESS THAN - 4");
         end if;

         if Vdc < 'Z' then
            Failed ("OVERLOADED LESS THAN - 5");
         end if;

         if Vdi < 0 then
            Failed ("OVERLOADED LESS THAN - 6");
         end if;

         if -14 + 5 /= -9 then
            Failed ("OVERLOADED PLUS - 2");
         end if;

         if Vi + 5 /= Integer'First then
            Failed ("OVERLOADED PLUS - 3");
         end if;

         if Vb + True /= False then
            Failed ("OVERLOADED PLUS - 4");
         end if;

         if Ve + E2 /= E1 then
            Failed ("OVERLOADED PLUS - 5");
         end if;

         if Denum'(E3) + E2 /= E2 then
            Failed ("OVERLOADED PLUS - 6");
         end if;

         if Vdc + 'B' /= 'A' then
            Failed ("OVERLOADED PLUS - 7");
         end if;

         if Vdi + 14 /= -19 then       -- -33 + 14
            Failed ("OVERLOADED PLUS - 8");
         end if;

         Vi  := E5;
         Vdc := E5;
         Ve  := E2;
         Vb  := E2;
         if Vi /= 1 or Vdc /= 'M' or Ve /= Enum'Val (Ident_Int (1)) or
           Vb /= False then
            Failed
              ("OVERLOADING OF ENUMERATION LITERALS " &
               "AND PREDEFINED SUBPROGRAMS");
         end if;
      end Pkg;
   begin
      declare
         use Pkg;
      begin
         if not (Vi + 5 < 11) then
            Failed ("INCORRECT VISIBILITY OF GENERIC OVERLOADING");
         end if;
      end;
   end;

   Result;
end Cc3012a;
