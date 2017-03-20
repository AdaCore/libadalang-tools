-- CC3127A.ADA

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
--     FOR A CONSTRAINED IN FORMAL PARAMETER HAVING A RECORD OR PRIVATE
--     TYPE WITH DISCRIMINANTS, CHECK THAT CONSTRAINT_ERROR IS RAISED
--     IF AND ONLY IF CORRESPONDING DISCRIMINANTS OF THE ACTUAL AND
--     FORMAL PARAMETER DO NOT HAVE THE SAME VALUES.

-- HISTORY:
--     LB   12/04/86  CREATED ORIGINAL TEST.
--     VCL  08/19/87  CORRECTED THE FORMAT OF THIS HEADER.

with Report; use Report;

procedure Cc3127a is

   type Int is range 1 .. 20;

begin
   Test
     ("CC3127A",
      "CORRESPONDING DISCRIMINANTS OF THE GENERIC " &
      "ACTUAL PARAMETER AND THE GENERIC FORMAL " &
      "PARAMETER MUST HAVE THE SAME VALUES.");
   begin
      declare
         type Rec (A : Int) is record
            Rint : Positive := 2;
         end record;
         subtype Con_Rec is Rec (4);

         generic
            Grec : in Con_Rec;
         package Pa is
            Nrec : Con_Rec := Grec;
         end Pa;
      begin
         begin
            declare
               Rvar : Rec (3);
               package Ab is new Pa (Rvar);
            begin
               Failed ("EXCEPTION NOT RAISED 1");
               Ab.Nrec.Rint := Ident_Int (Ab.Nrec.Rint);
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED 1");
         end;

         begin
            declare
               Svar : Rec (4);
               package Cd is new Pa (Svar);
            begin
               if Equal (3, 3) then
                  Cd.Nrec.Rint := Ident_Int (Cd.Nrec.Rint);
               end if;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED 2");
         end;
      end;

      declare
         package Ef is
            type Pri_Rec (G : Int) is private;
         private
            type Pri_Rec (G : Int) is record
               Pint : Positive := 2;
            end record;
         end Ef;
         subtype Cpri_Rec is Ef.Pri_Rec (4);

         generic
            Gen_Rec : in Cpri_Rec;
         package Gh is
            Ngen_Rec : Cpri_Rec := Gen_Rec;
         end Gh;

      begin
         begin
            declare
               Pvar : Ef.Pri_Rec (4);
               package Lm is new Gh (Pvar);
            begin
               if Equal (3, 3) then
                  Lm.Ngen_Rec := Lm.Ngen_Rec;
               end if;
            end;
         exception
            when others =>
               Failed ("EXCEPTION RAISED 3");
         end;

         begin
            declare
               Ptvar : Ef.Pri_Rec (5);
               package Pac is new Gh (Ptvar);
            begin
               Failed ("EXCEPTION NOT RAISED 4");
               if Equal (3, 5) then
                  Comment
                    ("DISCRIMINANT OF PAC.NGEN_REC IS " &
                     Int'Image (Pac.Ngen_Rec.G));
               end if;
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED 4");
         end;
      end;
   end;

   Result;

end Cc3127a;
