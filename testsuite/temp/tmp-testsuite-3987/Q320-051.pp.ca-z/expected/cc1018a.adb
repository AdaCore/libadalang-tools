-- CC1018A.ADA

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
-- CHECK THAT A FORMAL OUT PARAMETER OF A GENERIC FORMAL SUBPROGRAM CAN
-- HAVE A FORMAL LIMITED TYPE AND AN ARRAY TYPE WITH LIMITED COMPONENTS.

-- AH  10/3/86

with Report; use Report;
procedure Cc1018a is
   type Int is range 1 .. 10;
   type Arr is array (Integer range <>) of Int;
   Int_Obj : Int          := 4;
   Arr_Obj : Arr (1 .. 5) := (2, 8, 2, 8, 2);

   generic
      type Glp is limited private;
      type Garr is array (Integer range <>) of Glp;
      Lp_Obj : in out Glp;
      Ga_Obj : in out Garr;
      with procedure P (X : out Glp; Y : out Garr);
      with function Same (Left, Right : Glp) return Boolean;
   procedure Gen_Proc;

   procedure Get_Values (X1 : out Int; Y1 : out Arr) is
   begin
      X1 := 4;
      Y1 := (2, 8, 2, 8, 2);
   end Get_Values;

   function Same_Value (Left, Right : Int) return Boolean is
   begin
      return Left = Right;
   end Same_Value;

   procedure Gen_Proc is
      Lp : Glp;
      A  : Garr (1 .. 5);
   begin
      P (Lp, A);
      if not Same (Lp, Lp_Obj) then
         Failed ("LIMITED PRIVATE TYPE HAS INCORRECT VALUE");
      end if;

      for Index in A'Range loop
         if not Same (A (Index), Ga_Obj (Index)) then
            Failed ("LIMITED PRIVATE TYPE COMPONENT " & "HAS INCORRECT VALUE");
         end if;
      end loop;
   end Gen_Proc;

   procedure Test_Lp is new Gen_Proc
     (Int,
      Arr,
      Int_Obj,
      Arr_Obj,
      Get_Values,
      Same_Value);

begin
   Test
     ("CC1018A",
      "A GENERIC FORMAL SUBPROGRAM OUT PRARAMETER " &
      "CAN HAVE A LIMITED TYPE");
   Test_Lp;

   Result;
end Cc1018a;
