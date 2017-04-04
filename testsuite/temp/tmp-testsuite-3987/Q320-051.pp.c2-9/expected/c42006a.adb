-- C42006A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN A STRING LITERAL OF AN
-- ARRAY TYPE CONTAINS A CHARACTER THAT DOES NOT BELONG TO THE COMPONENT
-- SUBTYPE.

-- SPS 2/22/84
-- EDS 12/02/97  MODIFIED THE COMPONENT SUBTYPES SO THAT THEY ARE NON-STATIC.
-- EDS 7/14/98    AVOID OPTIMIZATION

with Report; use Report;
procedure C42006a is
begin

   Test
     ("C42006A",
      "CHECK THAT THE VALUES OF STRING LITERALS MUST" &
      " BELONG TO THE COMPONENT SUBTYPE.");

   declare

      type Char_Comp is ('A', 'B', 'C', 'D', 'E', 'F');

      Asciinul : Character := Ascii.Nul;
      subtype Non_Graphic_Char is Character range Asciinul .. Ascii.Bel;

      Bee : Char_Comp := 'B';
      type Char_String is
        array (Positive range <>) of Char_Comp range Bee .. 'C';
      type Non_Graphic_Char_String is
        array (Positive range <>) of Non_Graphic_Char;

      C_Str   : Char_String (1 .. 1);
      C_Str_5 : Char_String (1 .. 5)             := "BBBBB";
      N_G_Str : Non_Graphic_Char_String (1 .. 1) :=
        (others => Non_Graphic_Char'First);

   begin

      begin
         C_Str_5 := "BABCC";      -- 'A' NOT IN COMPONENT SUBTYPE.
         Failed
           ("CONSTRAINT_ERROR NOT RAISED - 1 " &
            Char_Comp'Image (C_Str_5 (1)));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("SOME EXCEPTION RAISED - 1");
      end;

      begin
         C_Str_5 := "BCBCD";      -- 'D' NOT IN COMPONENT SUBTYPE.
         Failed
           ("CONSTRAINT_ERROR NOT RAISED - 2 " &
            Char_Comp'Image (C_Str_5 (1)));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("SOME EXCEPTION RAISED - 2");
      end;

      begin
         N_G_Str := "Z";
         Failed
           ("CONSTRAINT_ERROR NOT RAISED - 3 " &
            Integer'Image (Character'Pos (N_G_Str (1))));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("SOME EXCEPTION RAISED - 3");
      end;

   end;

   Result;

end C42006a;
