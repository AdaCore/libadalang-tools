-- CE3809B.ADA

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
-- HISTORY:
--     CHECK THAT FIXED I/O GET CAN READ A VALUE FROM A STRING.
--     CHECK THAT END_ERROR IS RAISED WHEN CALLED WITH A NULL STRING
--     OR A STRING CONTAINING SPACES AND/OR HORIZONTAL TABULATION
--     CHARACTERS.  CHECK THAT LAST CONTAINS THE INDEX OF THE LAST
--     CHARACTER READ FROM THE STRING.

-- HISTORY:
--     SPS 10/07/82
--     SPS 12/14/82
--     JBG 12/21/82
--     DWC 09/15/87  ADDED CASE TO INCLUDE ONLY TABS IN STRING AND
--                   CHECKED THAT END_ERROR IS RAISED.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3809b is
begin

   Test
     ("CE3809B",
      "CHECK THAT FIXED_IO GET " & "OPERATES CORRECTLY ON STRINGS");

   declare
      type Fx is delta 0.001 range -2.0 .. 1_000.0;
      package Fxio is new Fixed_Io (Fx);
      use Fxio;
      X   : Fx;
      L   : Positive;
      Str : String (1 .. 10) := "   10.25  ";
   begin

-- LEFT-JUSTIFIED IN STRING, POSITIVE, NO EXPONENT
      begin
         Get ("896.5  ", X, L);
         if X /= 896.5 then
            Failed ("FIXED VALUE FROM STRING INCORRECT");
         end if;
      exception
         when Data_Error =>
            Failed ("DATA_ERROR RAISED - FIXED - 1");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - FIXED - 1");
      end;

      if L /= Ident_Int (5) then
         Failed
           ("VALUE OF LAST INCORRECT - FIXED - 1.  " &
            "LAST IS" &
            Integer'Image (L));
      end if;

-- STRING LITERAL WITH BLANKS
      begin
         Get ("   ", X, L);
         Failed ("END_ERROR NOT RAISED - FIXED - 2");
      exception
         when End_Error =>
            if L /= 5 then
               Failed
                 ("AFTER END_ERROR, VALUE OF LAST " &
                  "INCORRECT - 2.  LAST IS" &
                  Integer'Image (L));
            end if;
         when Data_Error =>
            Failed ("DATA_ERROR RAISED - FIXED - 2");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED - 2");
      end;

-- NULL STRING LITERAL
      begin
         Get ("", X, L);
         Failed ("END_ERROR NOT RAISED - FIXED - 3");
      exception
         when End_Error =>
            if L /= 5 then
               Failed
                 ("AFTER END_ERROR, VALUE OF LAST " &
                  "INCORRECT - 3.  LAST IS" &
                  Integer'Image (L));
            end if;
         when Data_Error =>
            Failed ("DATA_ERROR RAISED - FIXED - 3");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED - 3");
      end;

-- NULL SLICE
      begin
         Get (Str (5 .. Ident_Int (2)), X, L);
         Failed ("END_ERROR NOT RAISED - FIXED - 4");
      exception
         when End_Error =>
            if L /= 5 then
               Failed
                 ("AFTER END_ERROR, VALUE OF LAST " &
                  "INCORRECT - 4.  LAST IS" &
                  Integer'Image (L));
            end if;
         when Data_Error =>
            Failed ("DATA_ERROR RAISED - FIXED - 4");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED - 4");
      end;

-- SLICE WITH BLANKS
      begin
         Get (Str (Ident_Int (9) .. 10), X, L);
         Failed ("END_ERROR NOT RAISED - FIXED - 5");
      exception
         when End_Error =>
            if L /= Ident_Int (5) then
               Failed
                 ("AFTER END_ERROR, VALUE OF LAST " &
                  "INCORRECT - 5.  LAST IS" &
                  Integer'Image (L));
            end if;
         when Data_Error =>
            Failed ("DATA_ERROR RAISED - FIXED - 5");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED - 5");
      end;

-- NON-NULL SLICE
      begin
         Get (Str (2 .. Ident_Int (8)), X, L);
         if X /= 10.25 then
            Failed ("FIXED VALUE INCORRECT - 6");
         end if;
         if L /= 8 then
            Failed
              ("LAST INCORRECT FOR SLICE - 6.  " &
               "LAST IS" &
               Integer'Image (L));
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED - 6");
      end;

-- LEFT-JUSTIFIED, POSITIVE EXPONENT
      begin
         Get ("1.34E+02", X, L);
         if X /= 134.0 then
            Failed ("FIXED WITH EXP FROM STRING INCORRECT - 7");
         end if;

         if L /= 8 then
            Failed
              ("VALUE OF LAST INCORRECT - FIXED - 7.  " &
               "LAST IS" &
               Integer'Image (L));
         end if;
      exception
         when Data_Error =>
            Failed ("DATA_EROR RAISED - FIXED - 7");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - FIXED - 7");
      end;

-- RIGHT-JUSTIFIED, NEGATIVE EXPONENT
      begin
         Get (" 25.0E-2", X, L);
         if X /= 0.25 then
            Failed ("NEG EXPONENT INCORRECT - 8");
         end if;
         if L /= 8 then
            Failed ("LAST INCORRECT - 8.  " & "LAST IS" & Integer'Image (L));
         end if;
      exception
         when others =>
            Failed ("EXCEPTION RAISED - 8");
      end;

-- RIGHT-JUSTIFIED, NEGATIVE
      Get ("  -1.50", X, L);
      if X /= -1.5 then
         Failed ("FIXED IN RIGHT JUSTIFIED STRING INCORRECT - 9");
      end if;
      if L /= 7 then
         Failed ("LAST INCORRECT - 9.  " & "LAST IS" & Integer'Image (L));
      end if;

-- HORIZONTAL TAB WITH BLANK
      begin
         Get (" " & Ascii.Ht & "2.3E+2", X, L);
         if X /= 230.0 then
            Failed ("FIXED WITH TAB IN STRING INCORRECT - 10");
         end if;
         if L /= 8 then
            Failed
              ("LAST INCORRECT FOR TAB - 10.  " &
               "LAST IS" &
               Integer'Image (L));
         end if;
      exception
         when Data_Error =>
            Failed ("DATA_ERROR FOR STRING WITH TAB - 10");
         when others =>
            Failed ("EXCEPTION FOR STRING WITH TAB - 10");
      end;

-- HORIZONTAL TABS ONLY

      begin
         Get (Ascii.Ht & Ascii.Ht, X, L);
         Failed ("END_ERROR NOT RAISED - FIXED - 11");
      exception
         when End_Error =>
            if L /= Ident_Int (8) then
               Failed
                 ("AFTER END_ERROR, VALUE OF LAST " &
                  "INCORRECT - 11.  LAST IS" &
                  Integer'Image (L));
            end if;
         when Data_Error =>
            Failed ("DATA_ERROR RAISED - FIXED - 11");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED - 11");
      end;
   end;

   Result;

end Ce3809b;
