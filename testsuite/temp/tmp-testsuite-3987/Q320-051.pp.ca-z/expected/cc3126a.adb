-- CC3126A.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED IF AND ONLY IF THE ACTUAL
--     PARAMETER DOES NOT HAVE THE SAME NUMBER OF COMPONENTS
--     (PER DIMENSION) AS THE FORMAL PARAMETER. ALSO THAT FOR NULL
--     ARRAYS NO ERROR IS RAISED.

-- HISTORY:
--     LB  12/02/86
--     DWC 08/11/87  CHANGED HEADING FORMAT.
--     RJW 10/26/89  INITIALIZED VARIABLE H.

with Report; use Report;

procedure Cc3126a is

begin
   Test
     ("CC3126A",
      "GENERIC ACTUAL PARAMETER MUST HAVE THE SAME " &
      "NUMBER OF COMPONENTS (PER DIMENSION) AS THE " &
      "GENERIC FORMAL PARMETER");
   begin
      declare
         type Arry1 is array (Integer range <>) of Integer;
         subtype Arr is Arry1 (1 .. 10);

         generic
            Garr : in Arr;
         package P is
            Narr : Arr := Garr;
         end P;

      begin
         begin
            declare
               X : Arry1 (2 .. 11) := (2 .. 11 => 0);
               package Q is new P (X);
            begin
               Q.Narr (2) := 1;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED 1");
         end;

         begin
            declare
               S : Arry1 (1 .. 11) := (1 .. 11 => 0);
               package R is new P (S);
            begin
               Failed ("EXCEPTION NOT RAISED 2");
               R.Narr (1) := Ident_Int (R.Narr (1));
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED 2");
         end;

         begin
            declare
               G : Arry1 (1 .. 9) := (1 .. 9 => 0);
               package K is new P (G);
            begin
               Failed ("EXCEPTION NOT RAISED 3");
               if Equal (3, 3) then
                  K.Narr (1) := Ident_Int (K.Narr (1));
               end if;
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED 3");
         end;

         begin
            declare
               S : Arry1 (1 .. 11) := (1 .. 11 => 0);
               package F is new P (S (2 .. 11));
            begin
               F.Narr (2) := Ident_Int (F.Narr (2));
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED 4");
         end;
      end;

      declare
         subtype Str is String (1 .. 20);

         generic
            Gvar : in Str;
         package M is
            Nvar : Str := Gvar;
         end M;

      begin
         begin
            declare
               L : String (2 .. 15);
               package U is new M (L);
            begin
               Failed ("EXCEPTION NOT RAISED 5");
               U.Nvar (2) := Ident_Char (U.Nvar (2));
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED 5");
         end;

         begin
            declare
               H : String (1 .. 20) := (others => 'R');
               package J is new M (H);
            begin
               if Equal (3, 3) then
                  J.Nvar (2) := Ident_Char (J.Nvar (2));
               end if;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED 6");
         end;
      exception
         when others =>
            Failed ("UNEXPECTED ERROR RAISED STRINGS");
      end;

      declare
         type Narry is array (Integer range <>) of Integer;
         subtype Snarry is Narry (2 .. 0);

         generic
            Rd : in Snarry;
         package Ja is
            Cd : Snarry := Rd;
         end Ja;
      begin
         begin
            declare
               Ad : Narry (1 .. 0);
               package Pa is new Ja (Ad);
            begin
               if not Equal (0, Pa.Cd'Last) then
                  Failed ("PARAMETER ATTRIBUTE INCORRECT");
               end if;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED 7");
         end;
      exception
         when others =>
            Failed
              ("UNEXPECTED EXCEPTION RAISED FOR ARRAYS " & "WITH NULL RANGES");
      end;
   end;

   Result;

end Cc3126a;
