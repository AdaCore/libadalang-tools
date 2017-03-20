-- C37211B.ADA

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
-- PRIVATE TYPE, AND THE DISCRIMINANT CONSTRAINT OCCURS AFTER THE FULL
-- DECLARATION OF THE TYPE.

-- R.WILLIAMS 8/28/86
-- EDS        7/14/98    AVOID OPTIMIZATION

with Report; use Report;
procedure C37211b is

   subtype Lies is Boolean range False .. False;

   package Pkg is
      type Priv (L : Lies) is private;
      type Lim (L : Lies) is limited private;

   private
      type Priv (L : Lies) is record
         null;
      end record;

      type Lim (L : Lies) is record
         null;
      end record;
   end Pkg;

   use Pkg;

begin
   Test
     ("C37211B",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
      "A DISCRIMINANT CONSTRAINT IF A VALUE " &
      "SPECIFIED FOR A DISCRIMINANT DOES NOT LIE " &
      "IN THE RANGE OF THE DISCRIMINANT WHERE THE " &
      "TYPE MARK DENOTES A PRIVATE OR LIMITED " &
      "PRIVATE TYPE, AND THE DISCRIMINANT " &
      "CONSTRAINT OCCURS AFTER THE FULL " &
      "DECLARATION OF THE TYPE");

   begin
      declare
         subtype Subpriv is Priv (Ident_Bool (True));
      begin
         declare
            Sp : Subpriv;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF SUBTYPE SUBPRIV " &
               Boolean'Image (Sp.L));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT SP");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "SUBTYPE SUBPRIV");
   end;

   begin
      declare
         subtype Sublim is Lim (Ident_Bool (True));
      begin
         declare
            Sl : Sublim;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF SUBTYPE SUBLIM" &
               Boolean'Image (Sl.L));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT SL ");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "SUBTYPE SUBLIM");
   end;

   begin
      declare
         type Parr is array (1 .. 5) of Priv (Ident_Bool (True));
      begin
         declare
            Par : Parr;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE PARR " &
               Boolean'Image (Par (1).L));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT PAR");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE PARR");
   end;

   begin
      declare
         type Larr is array (1 .. 10) of Lim (Ident_Bool (True));
      begin
         declare
            Lar : Larr;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE LARR " &
               Boolean'Image (Lar (1).L));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT LAR");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE LARR");
   end;

   begin
      declare
         type Priv1 is record
            X : Priv (Ident_Bool (True));
         end record;

      begin
         declare
            P1 : Priv1;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE PRIV1 " &
               Boolean'Image (P1.X.L));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT P1");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE PRIV1");
   end;

   begin
      declare
         type Lim1 is record
            X : Lim (Ident_Bool (True));
         end record;

      begin
         declare
            L1 : Lim1;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE LIM1 " &
               Boolean'Image (L1.X.L));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT L1");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE LIM1");
   end;

   begin
      declare
         type Accpriv is access Priv (Ident_Bool (True));
      begin
         declare
            Acp : Accpriv;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE ACCPRIV " &
               Boolean'Image (Acp.L));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT ACP");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE ACCPRIV");
   end;

   begin
      declare
         type Acclim is access Lim (Ident_Bool (True));
      begin
         declare
            Acl : Acclim;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE ACCLIM " &
               Boolean'Image (Acl.L));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT ACL");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE ACCLIM");
   end;

   begin
      declare
         type Newpriv is new Priv (Ident_Bool (True));
      begin
         declare
            Np : Newpriv;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE NEWPRIV " &
               Boolean'Image (Np.L));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT NP");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE NEWPRIV");
   end;

   begin
      declare
         type Newlim is new Lim (Ident_Bool (True));
      begin
         declare
            Nl : Newlim;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE NEWLIM " &
               Boolean'Image (Nl.L));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT NL");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE NEWLIM");
   end;

   begin
      declare
         P : Priv (Ident_Bool (True));
      begin
         Failed
           ("NO EXCEPTION RAISED AT THE DECLARATION OF " &
            "P " &
            Boolean'Image (P.L));
      exception
         when others =>
            Failed ("EXCEPTION RAISED INSIDE BLOCK " & "CONTAINING P");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT DECLARATION OF " & "P");
   end;

   begin
      declare
         L : Lim (Ident_Bool (True));
      begin
         Failed
           ("NO EXCEPTION RAISED AT THE DECLARATION OF " &
            "L " &
            Boolean'Image (L.L));
      exception
         when others =>
            Failed ("EXCEPTION RAISED INSIDE BLOCK " & "CONTAINING L");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT DECLARATION OF " & "L");
   end;

   begin
      declare
         type Priv_Name is access Priv;
      begin
         declare
            Pn : Priv_Name := new Priv (Ident_Bool (True));
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "DECLARATION OF OBJECT PN " &
               Boolean'Image (Pn.L));
         exception
            when others =>
               Failed ("EXCEPTION ATTEMPTING TO USE OBJECT");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF OBJECT PN");
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED AT ELABORATION OF TYPE " & "PRIV_NAME");
   end;

   begin
      declare
         type Lim_Name is access Lim;
      begin
         declare
            Ln : Lim_Name := new Lim (Ident_Bool (True));
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "DECLARATION OF OBJECT LN " &
               Boolean'Image (Ln.L));
         exception
            when others =>
               Failed ("EXCEPTION ATTEMPTING TO USE OBJECT");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF OBJECT LN");
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED AT ELABORATION OF TYPE " & "LIM_NAME");
   end;

   begin
      declare
         package Pp is
            type Bad_Priv (D : Lies := Ident_Bool (True)) is private;
         private
            type Bad_Priv (D : Lies := Ident_Bool (True)) is record
               null;
            end record;
         end Pp;

         use Pp;
      begin
         declare
            Bp : Bad_Priv;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "DECLARATION OF OBJECT BP " &
               Boolean'Image (Bp.D));
         exception
            when others =>
               Failed ("EXCEPTION ATTEMPTING TO USE OBJECT");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF OBJECT BP");
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED AT ELABORATION OF TYPE " & "BAD_PRIV");
   end;

   begin
      declare
         package Pl is
            type Bad_Lim (D : Lies := Ident_Bool (True)) is limited private;
         private
            type Bad_Lim (D : Lies := Ident_Bool (True)) is record
               null;
            end record;
         end Pl;

         use Pl;
      begin
         declare
            Bl : Bad_Lim;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "DECLARATION OF OBJECT BL " &
               Boolean'Image (Bl.D));
         exception
            when others =>
               Failed ("EXCEPTION ATTEMPTING TO USE OBJECT");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF OBJECT BL");
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED AT ELABORATION OF TYPE " & "BAD_LIM");
   end;

   Result;
end C37211b;
