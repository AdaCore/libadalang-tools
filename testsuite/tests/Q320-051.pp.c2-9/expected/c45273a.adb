-- C45273A.ADA

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
--     CHECK THAT EQUALITY AND INEQUALITY ARE EVALUATED CORRECTLY FOR
--     RECORD OBJECTS HAVING DIFFERENT VALUES OF THE 'CONSTRAINED
--     ATTRIBUTE.

-- HISTORY:
--     TBN  08/07/86  CREATED ORIGINAL TEST.
--     VCL  10/27/87  MODIFIED THIS HEADER; RELOCATED THE CALL TO
--                    REPORT.TEST SO THAT IT COMES BEFORE ANY
--                    DECLARATIONS; CHANGED THE 'ELSEIF' CONDITION IN
--                    THE PROCEDURE 'PROC' SO THAT IT REFERS TO THE
--                    FORMAL PARAMETERS.

with Report; use Report;
procedure C45273a is
begin
   Test
     ("C45273A",
      "EQUALITY AND INEQUALITY ARE " &
      "EVALUATED CORRECTLY FOR RECORD OBJECTS HAVING " &
      "DIFFERENT VALUES OF THE 'CONSTRAINED' " & " ATTRIBUTE");

   declare
      subtype Int is Integer range 1 .. 20;
      type Rec_Type1 is record
         A : Integer;
      end record;

      type Rec_Type2 (Len : Int := 3) is record
         A : String (1 .. Len);
      end record;

      type Rec_Type3 (Num : Int := 1) is record
         A : Rec_Type1;
      end record;

      Rec1 : Rec_Type2 (3) := (3, "WHO");
      Rec2 : Rec_Type2;
      Rec3 : Rec_Type2 (5) := (5, "WHERE");
      Rec4 : Rec_Type3;
      Rec5 : Rec_Type3 (1) := (1, A => (A => 5));

      procedure Proc (Prec1 : Rec_Type2; Prec2 : in out Rec_Type2) is
      begin
         if not (Prec1'Constrained) or Prec2'Constrained then
            Failed ("INCORRECT RESULTS FROM 'CONSTRAINED " & "ATTRIBUTE - 6");
         elsif Prec1 /= Prec2 then
            Failed ("INCORRECT RESULTS FOR RECORDS - 6");
         end if;
         Prec2.A := "WHO";
      end Proc;

   begin
      Rec2.A := "WHO";
      if not (Rec1'Constrained) or Rec2'Constrained then
         Failed ("INCORRECT RESULTS FROM 'CONSTRAINED " & "ATTRIBUTE - 1");
      elsif Rec1 /= Rec2 then
         Failed ("INCORRECT RESULTS FOR RECORDS - 1");
      end if;

      if Rec2'Constrained or not (Rec3'Constrained) then
         Failed ("INCORRECT RESULTS FROM 'CONSTRAINED " & "ATTRIBUTE - 2");
      elsif Rec2 = Rec3 then
         Failed ("INCORRECT RESULTS FOR RECORDS - 2");
      end if;

      Rec2 := (5, "WHERE");
      if Rec2'Constrained or not (Rec3'Constrained) then
         Failed ("INCORRECT RESULTS FROM 'CONSTRAINED " & "ATTRIBUTE - 3");
      elsif Rec2 /= Rec3 then
         Failed ("INCORRECT RESULTS FOR RECORDS - 3");
      end if;

      Rec4.A.A := 5;
      if Rec4'Constrained or not (Rec5'Constrained) then
         Failed ("INCORRECT RESULTS FROM 'CONSTRAINED " & "ATTRIBUTE - 4");
      elsif Rec4 /= Rec5 then
         Failed ("INCORRECT RESULTS FOR RECORDS - 4");
      end if;

      Rec5.A := (A => 6);
      if Rec4'Constrained or not (Rec5'Constrained) then
         Failed ("INCORRECT RESULTS FROM 'CONSTRAINED " & "ATTRIBUTE - 5");
      elsif Rec4 = Rec5 then
         Failed ("INCORRECT RESULTS FOR RECORDS - 5");
      end if;

      Rec1.A := "WHY";
      Rec2   := (3, "WHY");
      Proc (Rec1, Rec2);
      if not (Rec1'Constrained) or Rec2'Constrained then
         Failed ("INCORRECT RESULTS FROM 'CONSTRAINED " & "ATTRIBUTE - 7");
      elsif Rec1 = Rec2 then
         Failed ("INCORRECT RESULTS FOR RECORDS - 7");
      end if;
   end;

   Result;
end C45273a;
