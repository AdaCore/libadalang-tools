-- C48007C.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T", CHECK THAT CONSTRAINT_ERROR IS
-- RAISED IF T IS A CONSTRAINED ARRAY TYPE AND AT LEAST ONE INDEX BOUND
-- FOR T DOES NOT EQUAL THE CORRESPONDING VALUE SPECIFIED FOR THE
-- ALLOCATOR'S BASE TYPE.

-- EG  08/10/84

with Report;

procedure C48007c is

   use Report;

begin

   Test
     ("C48007C",
      "FOR ALLOCATORS OF THE FORM 'NEW T' CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "APPROPRIATE - CONSTRAINED ARRAY TYPE");

   declare

      type Ua1 is array (Integer range <>) of Integer;
      type Ua2 is array (Integer range <>, Integer range <>) of Integer;
      type Ua3 is array (Integer range <>) of Ua1 (1 .. 2);

      subtype Ca11 is Ua1 (1 .. 3);
      subtype Ca12 is Ua1 (3 .. 2);
      subtype Ca21 is Ua2 (1 .. 2, 1 .. 2);
      subtype Ca22 is Ua2 (1 .. 2, 2 .. 0);
      subtype Ca31 is Ua3 (1 .. 2);
      subtype Ca32 is Ua3 (4 .. 1);

      type A_Ua11 is access Ua1 (2 .. 4);
      type A_Ua12 is access Ua1 (4 .. 3);
      type A_Ua21 is access Ua2 (1 .. 3, 1 .. 2);
      type A_Ua22 is access Ua2 (1 .. 2, 2 .. 1);
      type A_Ua31 is access Ua3 (1 .. 3);
      type A_Ua32 is access Ua3 (3 .. 1);

      V11 : A_Ua11;
      V12 : A_Ua12;
      V21 : A_Ua21;
      V22 : A_Ua22;
      V31 : A_Ua31;
      V32 : A_Ua32;

   begin

      begin -- V11

         V11 := new Ca11;
         Failed ("NO EXCEPTION RAISED - V11");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - V11");

      end;

      begin -- V12

         V12 := new Ca12;
         Failed ("NO EXCEPTION RAISED - V12");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - V12");

      end;

      begin -- V21

         V21 := new Ca21;
         Failed ("NO EXCEPTION RAISED - V21");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - V21");

      end;

      begin -- V22

         V22 := new Ca22;
         Failed ("NO EXCEPTION RAISED - V22");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - V22");

      end;

      begin -- V31

         V31 := new Ca31;
         Failed ("NO EXCEPTION RAISED - V31");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - V31");

      end;

      begin -- V32

         V32 := new Ca32;
         Failed ("NO EXCEPTION RAISED - V32");

      exception

         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - V32");

      end;

   end;

   Result;

end C48007c;
