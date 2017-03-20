-- C37403A.ADA

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
-- CHECK THAT WHEN A FORMAL PARAMETER OF A SUBPROGRAM, ENTRY, OR
-- GENERIC UNIT HAS AN UNCONSTRAINED TYPE WITH DISCRIMINANTS THAT DO
-- NOT HAVE DEFAULTS, 'CONSTRAINED IS 'TRUE' REGARDLESS OF THE MODE
-- OF THE PARAMETER.

-- R.WILLIAMS 9/1/86

with Report; use Report;
procedure C37403a is

begin
   Test
     ("C37403A",
      "CHECK THAT WHEN A FORMAL PARAMETER OF A " &
      "SUBPROGRAM, ENTRY, OR GENERIC UNIT HAS AN " &
      "UNCONSTRAINED TYPE WITH DISCRIMINANTS THAT " &
      "DO NOT HAVE DEFAULTS, 'CONSTRAINED IS " &
      "'TRUE' REGARDLESS OF THE MODE OF THE " &
      "PARAMETER");

   declare

      subtype Int is Integer range 1 .. 10;

      type Matrix is array (Int range <>, Int range <>) of Integer;

      type Square (Side : Int) is record
         Mat : Matrix (1 .. Side, 1 .. Side);
      end record;

      S1 : Square (2) := (2, ((1, 2), (3, 4)));

      S2 : Square (2) := S1;

      S3 : Square (2);

      Sc : constant Square := (Side => 1, Mat => (1 => (1 => 1)));

      procedure P
        (Pin1, Pin2 : in     Square;
         Pinout     : in out Square;
         Pout       :    out Square)
      is

      begin
         if Pin1'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR OBJECT " & "OF IN MODE - 1");
         end if;

         if Pin2'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR OBJECT " & "OF IN MODE - 2");
         end if;

         if Pinout'Constrained then
            null;
         else
            Failed
              ("'CONSTRAINED IS 'FALSE' FOR " & "OBJECT OF IN OUT MODE - 1");
         end if;

         if Pout'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR " & "OBJECT OF OUT MODE - 1");
         end if;

         Pout := (2, ((1, 2), (3, 4)));
      end P;

      task T is
         entry Q
           (Pin1, Pin2 : in     Square;
            Pinout     : in out Square;
            Pout       :    out Square);
      end T;

      task body T is
      begin
         accept Q
           (Pin1, Pin2 : in     Square;
            Pinout     : in out Square;
            Pout       :    out Square) do

            begin
               if Pin1'Constrained then
                  null;
               else
                  Failed
                    ("'CONSTRAINED IS 'FALSE' FOR " & "OBJECT OF IN MODE - 3");
               end if;

               if Pin2'Constrained then
                  null;
               else
                  Failed
                    ("'CONSTRAINED IS 'FALSE' FOR " & "OBJECT OF IN MODE - 4");
               end if;

               if Pinout'Constrained then
                  null;
               else
                  Failed
                    ("'CONSTRAINED IS 'FALSE' FOR " &
                     "OBJECT OF " &
                     "IN OUT MODE - 2");
               end if;

               if Pout'Constrained then
                  null;
               else
                  Failed
                    ("'CONSTRAINED IS 'FALSE' FOR " &
                     "OBJECT OF " &
                     "OUT MODE - 2");
               end if;

               Pout := (2, ((1, 2), (3, 4)));
            end;
         end Q;
      end T;

      generic
         Pin1, Pin2 : in Square;
         Pinout : in out Square;
      package R is
      end R;

      package body R is
      begin
         if Pin1'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR OBJECT " & "OF IN MODE - 5");
         end if;

         if Pin2'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR OBJECT " & "OF IN MODE - 6");
         end if;

         if Pinout'Constrained then
            null;
         else
            Failed
              ("'CONSTRAINED IS 'FALSE' FOR " & "OBJECT OF IN OUT MODE - 3");
         end if;

      end R;

      package S is new R (S1, Sc, S2);

   begin
      P (S1, Sc, S2, S3);
      T.Q (S1, Sc, S2, S3);
   end;

   Result;
end C37403a;
