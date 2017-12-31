-- C41303V.ADA

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
--                 ACC ACC REC  ||             |
--                --------------||-------------|--------------------
--       2 '.ALL'  ACC ACC ARR  ||             |     XXXXXXXXX
--                --------------||-------------|--------------------
--                 ACC ACC SCLR ||             |
--      ============================================================

-- RM  1/29/82
-- SPS 12/2/82

with Report; use Report;
procedure C41303v is

begin

   Test
     ("C41303V",
      "CHECK THAT IF  A  IS AN IDENTIFIER DENOTING" &
      " AN ACCESS OBJECT WHICH IN TURN DESIGNATES" &
      " AN ACCESS OBJECT,  THE FORM  A.ALL.ALL  IS" & " ACCEPTED");

   -------------------------------------------------------------------
   ---------------  ACCESS TO ACCESS TO ARRAY  -----------------------

   declare

      type Arr is array (1 .. 2) of Boolean;

      Arr_Const  : Arr := (True, False);
      Arr_Var    : Arr := Arr_Const;
      Arr_Var0   : Arr := Arr_Const;
      Arr_Const2 : Arr := (False, True);

      type Accarr is access Arr;

      type Acc_Accarr is access Accarr;

      Acc_Accarr_Var : Acc_Accarr := new Accarr'(new Arr'(Arr_Const2));

      Acc_Accarr_Var0 : Acc_Accarr := new Accarr'(new Arr'(Arr_Const2));

      procedure R_Assign (R_In : in Arr; R_Inout : in out Arr) is
      begin
         Arr_Var  := R_In;
         Arr_Var0 := R_Inout;
      end R_Assign;

      procedure L_Assign (L_Out : out Arr; L_Inout : in out Arr) is
      begin
         L_Out   := Arr_Const;
         L_Inout := Arr_Const;
      end L_Assign;

   begin

      R_Assign (Acc_Accarr_Var.all.all, Acc_Accarr_Var0.all.all);

      if Arr_Var /= Arr_Const2 then
         Failed ("ACC2 ARRAY, RIGHT SIDE (1), WRONG VAL.");
      end if;

      if Arr_Var0 /= Arr_Const2 then
         Failed ("ACC2 ARRAY, RIGHT SIDE (2), WRONG VAL.");
      end if;

      L_Assign (Acc_Accarr_Var.all.all, Acc_Accarr_Var0.all.all);

      if (True, False) /= Acc_Accarr_Var.all.all then
         Failed ("ACC2 ARRAY, LEFT SIDE (1), WRONG VAL.");
      end if;

      if (True, False) /= Acc_Accarr_Var0.all.all then
         Failed ("ACC2 ARRAY, LEFT SIDE (2), WRONG VAL.");
      end if;

   end;

   -------------------------------------------------------------------

   Result;

end C41303v;
