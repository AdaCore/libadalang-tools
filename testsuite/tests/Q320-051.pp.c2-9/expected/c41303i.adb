-- C41303I.ADA

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
-- CHECK THAT THE NOTATION L.ALL IS ALLOWED IF L IS THE NAME OF AN
--     ACCESS OBJECT DESIGNATING A RECORD, AN ARRAY, A SCALAR, OR
--     ANOTHER ACCESS OBJECT.
-- CHECK THAT IF A IS AN IDENTIFIER DENOTING AN ACCESS OBJECT WHICH
--     IN TURN DESIGNATES AN ACCESS OBJECT, THE FORM  A.ALL.ALL  IS
--     ACCEPTED.

-- THIS OBJECTIVE IS COVERED IN SEVERAL TESTS. IN THE FOLLOWING DIAGRAM,
--     THE PORTION COVERED BY THE CURRENT TEST IS MARKED BY 'X' .

--                              ||   ASSIGNMT  |  PROC. PARAMETERS
--                              ||  ():=  :=() | IN   OUT    IN OUT
--      ========================||=============|====================
--                 ACC REC      ||             |
--                --------------||-------------|--------------------
--       1 '.ALL'  ACC ARR      ||             |
--                --------------||-------------|--------------------
--                 ACC SCLR     ||             |
--      ========================||=============|====================
--                 ACC ACC REC  ||             |
--                --------------||-------------|--------------------
--       1 '.ALL'  ACC ACC ARR  ||             |
--                --------------||-------------|--------------------
--                 ACC ACC SCLR ||             |
--      ========================||=============|====================
--                 ACC ACC REC  ||  XXXXXXXXX  |
--                --------------||-------------|--------------------
--       2 '.ALL'  ACC ACC ARR  ||             |
--                --------------||-------------|--------------------
--                 ACC ACC SCLR ||             |
--      ============================================================

-- RM  1/20/82
-- RM  1/25/82
-- SPS 12/2/82

with Report; use Report;
procedure C41303i is

begin

   Test
     ("C41303I",
      "CHECK THAT IF  A  IS AN IDENTIFIER DENOTING" &
      " AN ACCESS OBJECT WHICH IN TURN DESIGNATES" &
      " AN ACCESS OBJECT,  THE FORM  A.ALL.ALL  IS" &
      " ACCEPTED");

   -------------------------------------------------------------------
   ---------------  ACCESS TO ACCESS TO RECORD  ----------------------

   declare

      type Rec is
 record
         A, B, C : Integer;
      end record;

      Rec_Const  : Rec := (7, 8, 9);
      Rec_Var    : Rec := Rec_Const;
      Rec_Const2 : Rec := (17, 18, 19);

      type Accrec is access Rec;

      type Acc_Accrec is access Accrec;

      Acc_Accrec_Var : Acc_Accrec := new Accrec'(new Rec'(Rec_Const2));

   begin

      Rec_Var := Acc_Accrec_Var.all.all;

      if Rec_Var /= Rec_Const2 then
         Failed ("ACC2 RECORD,RIGHT SIDE OF ASSIGN., WRONG VAL.");
      end if;

      Acc_Accrec_Var.all.all := Rec_Const;

      if (7, 8, 9) /= Acc_Accrec_Var.all.all then
         Failed ("ACC2 RECORD, LEFT SIDE OF ASSIGN., WRONG VAL.");
      end if;

   end;

   -------------------------------------------------------------------

   Result;

end C41303i;
