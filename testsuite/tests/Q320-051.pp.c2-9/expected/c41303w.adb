-- C41303W.ADA

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
--       2 '.ALL'  ACC ACC ARR  ||             |
--                --------------||-------------|--------------------
--                 ACC ACC SCLR ||             |     XXXXXXXXX
--      ============================================================

-- RM  1/29/82
-- SPS 12/2/82

with Report; use Report;
procedure C41303w is

begin

   Test
     ("C41303W",
      "CHECK THAT IF  A  IS AN IDENTIFIER DENOTING" &
      " AN ACCESS OBJECT WHICH IN TURN DESIGNATES" &
      " AN ACCESS OBJECT,  THE FORM  A.ALL.ALL  IS" & " ACCEPTED");

   -------------------------------------------------------------------
   ---------------  ACCESS TO ACCESS TO SCALAR  ----------------------

   declare

      type Newint is new Integer;

      Newint_Const  : Newint := (813);
      Newint_Var    : Newint := Newint_Const;
      Newint_Var0   : Newint := Newint_Const;
      Newint_Const2 : Newint := (707);

      type Accnewint is access Newint;

      type Acc_Accnewint is access Accnewint;

      Acc_Accnewint_Var : Acc_Accnewint :=
        new Accnewint'(new Newint'(Newint_Const2));

      Acc_Accnewint_Var0 : Acc_Accnewint :=
        new Accnewint'(new Newint'(Newint_Const2));

      procedure R_Assign (R_In : in Newint; R_Inout : in out Newint) is
      begin
         Newint_Var  := R_In;
         Newint_Var0 := R_Inout;
      end R_Assign;

      procedure L_Assign (L_Out : out Newint; L_Inout : in out Newint) is
      begin
         L_Out   := Newint_Const;
         L_Inout := Newint_Const;
      end L_Assign;

   begin

      R_Assign (Acc_Accnewint_Var.all.all, Acc_Accnewint_Var0.all.all);

      if Newint_Var /= Newint_Const2 then
         Failed ("ACC2 NEWINT, RIGHT SIDE (1), WRONG VAL.");
      end if;

      if Newint_Var0 /= Newint_Const2 then
         Failed ("ACC2 NEWINT, RIGHT SIDE (2), WRONG VAL.");
      end if;

      L_Assign (Acc_Accnewint_Var.all.all, Acc_Accnewint_Var0.all.all);

      if Newint_Const /= Acc_Accnewint_Var.all.all then
         Failed ("ACC2 NEWINT, LEFT SIDE (1), WRONG VAL.");
      end if;

      if Newint_Const /= Acc_Accnewint_Var0.all.all then
         Failed ("ACC2 NEWINT, LEFT SIDE (2), WRONG VAL.");
      end if;

   end;

   -------------------------------------------------------------------

   Result;

end C41303w;
