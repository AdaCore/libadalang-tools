-- C37402A.ADA

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
-- CHECK THAT WHEN A FORMAL PARAMETER OF A SUBPROGRAM, ENTRY, OR GENERIC
-- UNIT HAS AN UNCONSTRAINED TYPE WITH DISCRIMINANTS THAT HAVE DEFAULTS,
-- 'CONSTRAINED IS 'TRUE' WHEN APPLIED TO FORMAL PARAMETERS OF MODE IN AND
-- HAS THE VALUE OF THE ACTUAL PARAMETER FOR THE OTHER MODES.

-- R.WILLIAMS 9/1/86

with Report; use Report;
procedure C37402a is

begin
   Test
     ("C37402A",
      "CHECK THAT WHEN A FORMAL PARAMETER OF A " &
      "SUBPROGRAM, ENTRY, OR GENERIC UNIT HAS AN " &
      "UNCONSTRAINED TYPE WITH DISCRIMINANTS THAT " &
      "HAVE DEFAULTS, 'CONSTRAINED IS 'TRUE' WHEN " &
      "APPLIED TO FORMAL PARAMETERS OF MODE IN " &
      "AND HAS THE VALUE OF THE ACTUAL PARAMETER " & "FOR THE OTHER MODES");

   declare

      subtype Int is Integer range 1 .. 5;

      type Matrix is array (Int range <>, Int range <>) of Integer;

      type Square (Side : Int := 1) is record
         Mat : Matrix (1 .. Side, 1 .. Side);
      end record;

      Sc : constant Square := (2, ((0, 0), (0, 0)));

      Ac : Square (2) := (2, ((1, 2), (3, 4)));
      Au : Square     := (Side => 1, Mat => (1 => (1 => 1)));

      Bc : Square (2) := Ac;
      Bu : Square     := Au;

      Cc : Square (2);
      Cu : Square;

      procedure P
        (Con, In_Con : in     Square; Inout_Con : in out Square;
         Out_Con : out Square; In_Unc : in Square; Inout_Unc : in out Square;
         Out_Unc     :    out Square)
      is

      begin
         if Con'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR OBJECT " & "OF IN MODE - 1");
         end if;

         if In_Con'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR OBJECT " & "OF IN MODE - 2");
         end if;

         if In_Unc'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR OBJECT " & "OF IN MODE - 3");
         end if;

         if Inout_Con'Constrained then
            null;
         else
            Failed
              ("'CONSTRAINED IS 'FALSE' FOR " &
               "CONSTRAINED OBJECT OF IN OUT MODE - 1");
         end if;

         if Out_Con'Constrained then
            null;
         else
            Failed
              ("'CONSTRAINED IS 'FALSE' FOR " &
               "CONSTRAINED OBJECT OF OUT MODE - 1");
         end if;

         if Inout_Unc'Constrained then
            Failed
              ("'CONSTRAINED IS 'TRUE' FOR " &
               "UNCONSTRAINED OBJECT OF IN OUT MODE " & "- 1");
         end if;

         if Out_Unc'Constrained then
            Failed
              ("'CONSTRAINED IS 'TRUE' FOR " &
               "UNCONSTRAINED OBJECT OF OUT MODE - 1");
         end if;

         Out_Con := (2, ((1, 2), (3, 4)));
         Out_Unc := (2, ((1, 2), (3, 4)));
      end P;

      task T is
         entry Q
           (Con, In_Con : in     Square; Inout_Con : in out Square;
            Out_Con     :    out Square; In_Unc : in Square;
            Inout_Unc   : in out Square; Out_Unc : out Square);
      end T;

      task body T is
      begin
         accept Q
           (Con, In_Con : in     Square; Inout_Con : in out Square;
            Out_Con     :    out Square; In_Unc : in Square;
            Inout_Unc   : in out Square; Out_Unc : out Square)
         do
            begin
               if Con'Constrained then
                  null;
               else
                  Failed
                    ("'CONSTRAINED IS 'FALSE' FOR " & "OBJECT OF IN MODE - 4");
               end if;

               if In_Con'Constrained then
                  null;
               else
                  Failed
                    ("'CONSTRAINED IS 'FALSE' FOR " & "OBJECT OF IN MODE - 5");
               end if;

               if In_Unc'Constrained then
                  null;
               else
                  Failed
                    ("'CONSTRAINED IS 'FALSE' FOR " & "OBJECT OF IN MODE - 6");
               end if;

               if Inout_Con'Constrained then
                  null;
               else
                  Failed
                    ("'CONSTRAINED IS 'FALSE' FOR " &
                     "CONSTRAINED OBJECT OF " & "IN OUT MODE - 2");
               end if;

               if Out_Con'Constrained then
                  null;
               else
                  Failed
                    ("'CONSTRAINED IS 'FALSE' FOR " &
                     "CONSTRAINED OBJECT OF " & "OUT MODE - 2");
               end if;

               if Inout_Unc'Constrained then
                  Failed
                    ("'CONSTRAINED IS 'TRUE' FOR " &
                     "UNCONSTRAINED OBJECT OF " & "IN OUT MODE - 2");
               end if;

               if Out_Unc'Constrained then
                  Failed
                    ("'CONSTRAINED IS 'TRUE' FOR " &
                     "UNCONSTRAINED OBJECT OF " & "OUT MODE - 2");
               end if;

               Out_Con := (2, ((1, 2), (3, 4)));
               Out_Unc := (2, ((1, 2), (3, 4)));
            end;
         end Q;
      end T;

      generic
         Con, In_Con : in Square;
         Inout_Con : in out Square;
         In_Unc : in Square;
         Inout_Unc : in out Square;
      package R is
      end R;

      package body R is
      begin
         if Con'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR OBJECT " & "OF IN MODE - 7");
         end if;

         if In_Con'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR OBJECT " & "OF IN MODE - 8");
         end if;

         if In_Unc'Constrained then
            null;
         else
            Failed ("'CONSTRAINED IS 'FALSE' FOR OBJECT " & "OF IN MODE - 9");
         end if;

         if Inout_Con'Constrained then
            null;
         else
            Failed
              ("'CONSTRAINED IS 'FALSE' FOR " &
               "CONSTRAINED OBJECT OF IN OUT MODE - 3");
         end if;

         if Inout_Unc'Constrained then
            Failed
              ("'CONSTRAINED IS 'TRUE' FOR " &
               "UNCONSTRAINED OBJECT OF IN OUT MODE " & "- 3");
         end if;

      end R;

      package S is new R (Sc, Ac, Bc, Au, Bu);

   begin
      P (Sc, Ac, Bc, Cc, Au, Bu, Cu);
      T.Q (Sc, Ac, Bc, Cc, Au, Bu, Cu);
   end;

   Result;
end C37402a;
