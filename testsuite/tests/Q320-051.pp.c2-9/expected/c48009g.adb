-- C48009G.ADA

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
--     FOR ALLOCATORS OF THE FORM "NEW T'(X)", CHECK THAT
--     CONSTRAINT_ERROR IS RAISED IF T IS A CONSTRAINED ACCESS
--     TYPE AND THE OBJECT DESIGNATED BY X DOES NOT HAVE DISCRIMINANTS
--     OR INDEX BOUNDS THAT EQUAL THE CORRESPONDING VALUES FOR T.

-- HISTORY:
--     EG  08/30/84  CREATED ORIGINAL TEST.
--     JET 01/05/87  UPDATED HEADER FORMAT AND ADDED CODE TO PREVENT
--                   OPTIMIZATION.

with Report;

procedure C48009g is

   use Report;

   generic
      type G_Type is private;
   function Equal_G (X : G_Type; Y : G_Type) return Boolean;

   function Equal_G (X : G_Type; Y : G_Type) return Boolean is
   begin
      if (Ident_Int (3) = 3) and (X = Y) then
         return True;
      else
         return False;
      end if;
   end Equal_G;

begin

   Test
     ("C48009G",
      "FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - CONSTRAINED ACCESS TYPE");

   declare

      type Int is range 1 .. 5;

      type Ur (A : Int) is record
         B : Integer;
      end record;
      type Ua is array (Int range <>) of Integer;

      package P is
         type Up (A, B : Int) is private;
         type Ul (A, B : Int) is limited private;
         Cons_Up : constant Up;
      private
         type Up (A, B : Int) is record
            C : Integer;
         end record;
         type Ul (A, B : Int) is record
            C : Integer;
         end record;
         Cons_Up : constant Up := (2, 2, (Ident_Int (3)));
      end P;

      type A_Ur is access Ur;
      type A_Ua is access Ua;
      type A_Up is access P.Up;
      type A_Ul is access P.Ul;

      subtype Ca_Ur is A_Ur (2);
      subtype Ca_Ua is A_Ua (2 .. 3);
      subtype Ca_Up is A_Up (3, 2);
      subtype Ca_Ul is A_Ul (2, 4);

      type A_Ca_Ur is access Ca_Ur;
      type A_Ca_Ua is access Ca_Ua;
      type A_Ca_Up is access Ca_Up;
      type A_Ca_Ul is access Ca_Ul;

      V_A_Ca_Ur : A_Ca_Ur;
      V_A_Ca_Ua : A_Ca_Ua;
      V_A_Ca_Up : A_Ca_Up;
      V_A_Ca_Ul : A_Ca_Ul;

      function Equal is new Equal_G (A_Ca_Ur);
      function Equal is new Equal_G (A_Ca_Ua);
      function Equal is new Equal_G (A_Ca_Up);
      function Equal is new Equal_G (A_Ca_Ul);

   begin

      begin
         V_A_Ca_Ur := new Ca_Ur'(new Ur'(1, (Ident_Int (2))));

         if Equal (V_A_Ca_Ur, V_A_Ca_Ur) then
            Failed ("NO EXCEPTION RAISED - UR");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UR");
      end;

      begin
         V_A_Ca_Ua := new Ca_Ua'(new Ua'(1 => 2, 2 => Ident_Int (3)));

         if Equal (V_A_Ca_Ua, V_A_Ca_Ua) then
            Failed ("NO EXCEPTION RAISED - UA");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UA");
      end;

      begin
         V_A_Ca_Up := new Ca_Up'(new P.Up'(P.Cons_Up));

         if Equal (V_A_Ca_Up, V_A_Ca_Up) then
            Failed ("NO EXCEPTION RAISED - UP");
         end if;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - UP");
      end;

      begin
         V_A_Ca_Ur := new Ca_Ur'(null);

         if not Equal (V_A_Ca_Ur, V_A_Ca_Ur) then
            Comment ("NO EXCEPTION RAISED - UR");
         end if;

      exception
         when others =>
            Failed ("EXCEPTION RAISED - UR");
      end;

      begin
         V_A_Ca_Ua := new Ca_Ua'(null);

         if not Equal (V_A_Ca_Ua, V_A_Ca_Ua) then
            Comment ("NO EXCEPTION RAISED - UA");
         end if;

      exception
         when others =>
            Failed ("EXCEPTION RAISED - UA");
      end;

      begin
         V_A_Ca_Up := new Ca_Up'(null);

         if not Equal (V_A_Ca_Up, V_A_Ca_Up) then
            Comment ("NO EXCEPTION RAISED - UP");
         end if;

      exception
         when others =>
            Failed ("EXCEPTION RAISED - UP");
      end;

      begin
         V_A_Ca_Ul := new Ca_Ul'(null);

         if not Equal (V_A_Ca_Ul, V_A_Ca_Ul) then
            Comment ("NO EXCEPTION RAISED - UL");
         end if;

      exception
         when others =>
            Failed ("EXCEPTION RAISED - UL");
      end;

   end;

   Result;

end C48009g;
