-- C36203A.ADA

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
-- CHECK THAT 'LENGTH YIELDS A RESULT OF TYPE UNIVERSAL INTEGER.

-- L.BROWN  07/31/86

with Report; use Report;
procedure C36203a is

   type Nint is new Integer range 1 .. 5;

   type Int_Arr is array (Integer range 1 .. 3) of Integer;
   type Int2_Arr is
     array (Integer range 1 .. 3, Integer range 1 .. 2) of Integer;

   Obja : Integer := 3;
   Objb : Nint    := 3;

begin
   Test ("C36203A", "'LENGTH YIELDS A RESULT OF TYPE " & "UNIVERSAL INTEGER");
   if (Obja + Int_Arr'Length) /= Ident_Int (6) then
      Failed
        ("INCORRECT VALUE RETURNED BY 'LENGTH " & "FOR ONE-DIM ARRAY TYPE 1");
   end if;

   if (Objb + Int_Arr'Length) /= 6 then
      Failed
        ("INCORRECT VALUE RETURNED BY 'LENGTH " & "FOR ONE-DIM ARRAY TYPE 2");
   end if;

   if (Obja + Int2_Arr'Length (1)) /= Ident_Int (6) then
      Failed
        ("INCORRECT VALUE RETURNED BY 'LENGTH " &
         "FOR FIRST DIMENSION OF TWO-DIM ARRAY TYPE 1");
   end if;

   if (Objb + Int2_Arr'Length (1)) /= 6 then
      Failed
        ("INCORRECT VALUE RETURNED BY 'LENGTH " &
         "FOR FIRST DIMENSION OF TWO-DIM ARRAY TYPE 2");
   end if;

   if (Obja + Int2_Arr'Length (2)) /= Ident_Int (5) then
      Failed
        ("INCORRECT VALUE RETURNED BY 'LENGTH " &
         "FOR SECOND DIMENSION OF TWO-DIM ARRAY TYPE 1");
   end if;

   if (Objb + Int2_Arr'Length (2)) /= 5 then
      Failed
        ("INCORRECT VALUE RETURNED BY 'LENGTH " &
         "FOR SECOND DIMENSION OF TWO-DIM ARRAY TYPE 2");
   end if;

   Result;

end C36203a;
