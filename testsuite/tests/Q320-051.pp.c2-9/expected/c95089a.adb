-- C95089A.ADA

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
-- CHECK THAT ALL PERMITTED FORMS OF VARIABLE NAMES ARE PERMITTED AS ACTUAL
-- PARAMETERS.

-- GLH 7/25/85

with Report; use Report;
procedure C95089a is

   subtype Int is Integer range 1 .. 3;

   type Rec (N : Int) is record
      S : String (1 .. N);
   end record;

   type Ptrstr is access String;

   R1, R2, R3 : Rec (3);
   S1, S2, S3 : String (1 .. 3);
   Ptrtbl     : array (1 .. 3) of Ptrstr;

   task T1 is
      entry E1 (S1 : in String; S2 : in out String; S3 : out String);
   end T1;

   task body T1 is
   begin
      loop
         select
            accept E1 (S1 : in String; S2 : in out String; S3 : out String) do
               S3 := S2;
               S2 := S1;
            end E1;
         or
            terminate;
         end select;
      end loop;
   end T1;

   task T2 is
      entry E2 (C1 : in Character; C2 : in out Character; C3 : out Character);
   end T2;

   task body T2 is
   begin
      loop
         select
            accept E2
              (C1 : in     Character;
               C2 : in out Character;
               C3 :    out Character) do
               C3 := C2;
               C2 := C1;
            end E2;
         or
            terminate;
         end select;
      end loop;
   end T2;

   function F1 (X : Int) return Ptrstr is
   begin
      return Ptrtbl (X);
   end F1;

   function "+" (S1, S2 : String) return Ptrstr is
   begin
      return Ptrtbl (Character'Pos (S1 (1)) - Character'Pos ('A') + 1);
   end "+";

begin

   Test
     ("C95089A",
      "CHECK THAT ALL PERMITTED FORMS OF VARIABLE " &
      "NAMES ARE PERMITTED AS ACTUAL PARAMETERS");

   S1 := "AAA";
   S2 := "BBB";
   T1.E1 (S1, S2, S3);
   if S2 /= "AAA" or S3 /= "BBB" then
      Failed ("SIMPLE VARIABLE AS AN ACTUAL PARAMETER NOT WORKING");
   end if;

   S1 := "AAA";
   S2 := "BBB";
   S3 := Ident_Str ("CCC");
   T2.E2 (S1 (1), S2 (Ident_Int (1)), S3 (1));
   if S2 /= "ABB" or S3 /= "BCC" then
      Failed ("INDEXED COMPONENT AS AN ACTUAL PARAMETER NOT " & "WORKING");
   end if;

   R1.S := "AAA";
   R2.S := "BBB";
   T1.E1 (R1.S, R2.S, R3.S);
   if R2.S /= "AAA" or R3.S /= "BBB" then
      Failed ("SELECTED COMPONENT AS AN ACTUAL PARAMETER " & "NOT WORKING");
   end if;

   S1 := "AAA";
   S2 := "BBB";
   T1.E1
     (S1 (1 .. Ident_Int (2)),
      S2 (1 .. 2),
      S3 (Ident_Int (1) .. Ident_Int (2)));
   if S2 /= "AAB" or S3 /= "BBC" then
      Failed ("SLICE AS AN ACTUAL PARAMETER NOT WORKING");
   end if;

   Ptrtbl (1) := new String'("AAA");
   Ptrtbl (2) := new String'("BBB");
   Ptrtbl (3) := new String'("CCC");
   T1.E1 (F1 (1).all, F1 (2).all, F1 (Ident_Int (3)).all);
   if Ptrtbl (2).all /= "AAA" or Ptrtbl (3).all /= "BBB" then
      Failed
        ("SELECTED COMPONENT OF FUNCTION VALUE AS AN ACTUAL " &
         "PARAMETER NOT WORKING");
   end if;

   Ptrtbl (1) := new String'("AAA");
   Ptrtbl (2) := new String'("BBB");
   Ptrtbl (3) := new String'("CCC");
   S1         := Ident_Str ("AAA");
   S2         := Ident_Str ("BBB");
   S3         := Ident_Str ("CCC");
   T1.E1 ("+" (S1, S1).all, "+" (S2, S2).all, "+" (S3, S3).all);
   if Ptrtbl (2).all /= "AAA" or Ptrtbl (3).all /= "BBB" then
      Failed
        ("SELECTED COMPONENT OF OVERLOADED OPERATOR " &
         "FUNCTION VALUE AS AN ACTUAL PARAMETER NOT WORKING");
   end if;

   Ptrtbl (1) := new String'("AAA");
   Ptrtbl (2) := new String'("BBB");
   Ptrtbl (3) := new String'("CCC");
   T2.E2 (F1 (1) (1), F1 (Ident_Int (2)) (1), F1 (3) (Ident_Int (1)));
   if Ptrtbl (2).all /= "ABB" or Ptrtbl (3).all /= "BCC" then
      Failed
        ("INDEXED COMPONENT OF FUNCTION VALUE AS AN ACTUAL " &
         "PARAMETER NOT WORKING");
   end if;

   Ptrtbl (1) := new String'("AAA");
   Ptrtbl (2) := new String'("BBB");
   Ptrtbl (3) := new String'("CCC");
   T1.E1
     (F1 (1) (2 .. 3),
      F1 (2) (Ident_Int (2) .. 3),
      F1 (3) (2 .. Ident_Int (3)));
   if Ptrtbl (2).all /= "BAA" or Ptrtbl (3).all /= "CBB" then
      Failed
        ("SLICE OF FUNCTION VALUE AS AN ACTUAL PARAMETER " & "NOT WORKING");
   end if;

   Result;

end C95089a;
