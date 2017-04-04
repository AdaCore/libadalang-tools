-- C37211C.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED BY A DISCRIMINANT CONSTRAINT
-- IF A VALUE SPECIFIED FOR A DISCRIMINANT DOES NOT LIE IN THE RANGE
-- OF THE DISCRIMINANT. THIS TEST CONTAINS CHECKS FOR SUBTYPE
-- INDICATIONS WHERE THE TYPE MARK DENOTES A PRIVATE OR LIMITED
-- PRIVATE TYPE, THE DISCRIMINANT CONSTRAINT OCCURS BEFORE THE FULL
-- DECLARATION OF THE TYPE, AND THERE ARE NO COMPONENTS OF THE TYPE
-- DEPENDENT ON THE DISCRIMINANT.

-- R.WILLIAMS 8/28/86
-- EDS        7/14/98    AVOID OPTIMIZATION

with Report; use Report;
procedure C37211c is

   Global : Boolean;

   subtype Lies is Boolean range False .. False;

   function Switch (B : Boolean) return Boolean is
   begin
      Global := B;
      return B;
   end Switch;

begin
   Test
     ("C37211C",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
      "A DISCRIMINANT CONSTRAINT IF A VALUE " &
      "SPECIFIED FOR A DISCRIMINANT DOES NOT LIE " &
      "IN THE RANGE OF THE DISCRIMINANT WHERE THE " &
      "TYPE MARK DENOTES A PRIVATE OR LIMITED " &
      "PRIVATE TYPE, AND THE DISCRIMINANT " &
      "CONSTRAINT OCCURS BEFORE THE FULL " &
      "DECLARATION OF THE TYPE");

   begin
      declare

         B1 : Boolean := Switch (True);

         package Pp is
            type Priv1 (D : Lies) is private;
            subtype Subpriv is Priv1 (Ident_Bool (True));

            B2 : Boolean := Switch (False);

         private
            type Priv1 (D : Lies) is record
               null;
            end record;
         end Pp;

         use Pp;
      begin
         declare
            Sp : Subpriv;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF SUBTYPE SUBPRIV " &
               Boolean'Image (Sp.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT SP");
      end;

   exception
      when Constraint_Error =>
         if Global then
            null;
         else
            Failed
              ("EXCEPTION RAISED AT ELABORATION OF " &
               "FULL TYPE PRIV1 NOT SUBTYPE SUBPRIV");
         end if;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "SUBTYPE SUBPRIV");
   end;

   begin
      declare

         B1 : Boolean := Switch (True);

         package Pl is
            type Lim1 (D : Lies) is limited private;
            subtype Sublim is Lim1 (Ident_Bool (True));

            B2 : Boolean := Switch (False);

         private
            type Lim1 (D : Lies) is record
               null;
            end record;
         end Pl;

         use Pl;
      begin
         declare
            Sl : Sublim;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF SUBTYPE SUBLIM " &
               Boolean'Image (Sl.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT SL");
      end;

   exception
      when Constraint_Error =>
         if Global then
            null;
         else
            Failed
              ("EXCEPTION RAISED AT ELABORATION OF " &
               "FULL TYPE LIM1 NOT SUBTYPE SUBLIM");
         end if;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "SUBTYPE SUBLIM");
   end;

   begin
      declare
         B1 : Boolean := Switch (True);

         package Pp is
            type Priv2 (D : Lies) is private;
            type Parr is array (1 .. 5) of Priv2 (Ident_Bool (True));

            B2 : Boolean := Switch (False);

         private
            type Priv2 (D : Lies) is record
               null;
            end record;
         end Pp;

         use Pp;
      begin
         declare
            Par : Parr;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE PARR " &
               Boolean'Image (Par (1).D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT PAR");
      end;

   exception
      when Constraint_Error =>
         if Global then
            null;
         else
            Failed
              ("EXCEPTION RAISED AT ELABORATION OF " &
               "FULL TYPE PRIV2 NOT TYPE PARR");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE PARR");
   end;

   begin
      declare
         B1 : Boolean := Switch (True);

         package Pl is
            type Lim2 (D : Lies) is limited private;
            type Larr is array (1 .. 5) of Lim2 (Ident_Bool (True));

            B2 : Boolean := Switch (False);

         private
            type Lim2 (D : Lies) is record
               null;
            end record;
         end Pl;

         use Pl;
      begin
         declare
            Lar : Larr;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE LARR " &
               Boolean'Image (Lar (1).D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT LAR");
      end;

   exception
      when Constraint_Error =>
         if Global then
            null;
         else
            Failed
              ("EXCEPTION RAISED AT ELABORATION OF " &
               "FULL TYPE LIM2 NOT TYPE LARR");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE LARR");
   end;

   begin
      declare
         B1 : Boolean := Switch (True);

         package Pp is
            type Priv3 (D : Lies) is private;

            type Priv4 is record
               X : Priv3 (Ident_Bool (True));
            end record;

            B2 : Boolean := Switch (False);

         private
            type Priv3 (D : Lies) is record
               null;
            end record;
         end Pp;

         use Pp;
      begin
         declare
            P4 : Priv4;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE PRIV4 " &
               Boolean'Image (P4.X.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT P4");
      end;

   exception
      when Constraint_Error =>
         if Global then
            null;
         else
            Failed
              ("EXCEPTION RAISED AT ELABORATION OF " &
               "FULL TYPE PRIV3 NOT TYPE PRIV4");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE PRIV4");
   end;

   begin
      declare
         B1 : Boolean := Switch (True);

         package Pl is
            type Lim3 (D : Lies) is limited private;

            type Lim4 is record
               X : Lim3 (Ident_Bool (True));
            end record;

            B2 : Boolean := Switch (False);

         private
            type Lim3 (D : Lies) is record
               null;
            end record;
         end Pl;

         use Pl;
      begin
         declare
            L4 : Lim4;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE LIM4 " &
               Boolean'Image (L4.X.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT L4");
      end;

   exception
      when Constraint_Error =>
         if Global then
            null;
         else
            Failed
              ("EXCEPTION RAISED AT ELABORATION OF " &
               "FULL TYPE LIM3 NOT TYPE LIM4");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE LIM4");
   end;

   begin
      declare
         B1 : Boolean := Switch (True);

         package Pp is
            type Priv5 (D : Lies) is private;
            type Accpriv is access Priv5 (Ident_Bool (True));

            B2 : Boolean := Switch (False);

         private
            type Priv5 (D : Lies) is record
               null;
            end record;
         end Pp;

         use Pp;

      begin
         declare
            Acp : Accpriv;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE ACCPRIV " &
               Boolean'Image (Acp.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT ACP");
      end;

   exception
      when Constraint_Error =>
         if Global then
            null;
         else
            Failed
              ("EXCEPTION RAISED AT ELABORATION OF " &
               "FULL TYPE PRIV5 NOT TYPE ACCPRIV");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE ACCPRIV");
   end;

   begin
      declare
         B1 : Boolean := Switch (True);

         package Pl is
            type Lim5 (D : Lies) is limited private;
            type Acclim is access Lim5 (Ident_Bool (True));

            B2 : Boolean := Switch (False);

         private
            type Lim5 (D : Lies) is record
               null;
            end record;
         end Pl;

         use Pl;

      begin
         declare
            Acl : Acclim;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE ACCLIM " &
               Boolean'Image (Acl.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT ACL");
      end;

   exception
      when Constraint_Error =>
         if Global then
            null;
         else
            Failed
              ("EXCEPTION RAISED AT ELABORATION OF " &
               "FULL TYPE LIM5 NOT TYPE ACCLIM");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE ACCLIM");
   end;

   Result;
end C37211c;
