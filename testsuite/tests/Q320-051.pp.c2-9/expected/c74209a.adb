-- C74209A.ADA

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
-- CHECK THAT OUTSIDE A PACKAGE WHICH DEFINES PRIVATE TYPES AND LIMITED
--    PRIVATE TYPES  IT IS POSSIBLE TO DECLARE SUBPROGRAMS WHICH USE
--    THOSE TYPES AS TYPES FOR PARAMETERS (OF ANY MODE EXCEPT OUT FOR A
--    LIMITED TYPE) OR AS THE TYPE FOR THE RESULT (FOR FUNCTION
--    SUBPROGRAMS).

-- RM 07/14/81

with Report;
procedure C74209a is

   use Report;

begin

   Test
     ("C74209A", "CHECK THAT PROCEDURE SIGNATURES CAN USE " & "PRIVATE TYPES");

   declare

      package Pack is

         type Lim_Priv is limited private;
         type Priv is private;
         Priv_Const_In  : constant Priv;
         Priv_Const_Out : constant Priv;
         function Packaged (X : in Integer) return Lim_Priv;
         function Equals (X, Y : Lim_Priv) return Boolean;
         procedure Assign (X : in Lim_Priv; Y : out Lim_Priv);

      private

         type Lim_Priv is new Integer;
         type Priv is new String (1 .. 5);
         Priv_Const_In  : constant Priv := "ABCDE";
         Priv_Const_Out : constant Priv := "FGHIJ";

      end Pack;

      Priv_Var_1, Priv_Var_2         : Pack.Priv;
      Lim_Priv_Var_1, Lim_Priv_Var_2 : Pack.Lim_Priv;

      use Pack;

      package body Pack is

         function Packaged (X : in Integer) return Lim_Priv is
         begin
            return Lim_Priv (X);
         end Packaged;

         function Equals (X, Y : Lim_Priv) return Boolean is
         begin
            return X = Y;
         end Equals;

         procedure Assign (X : in Lim_Priv; Y : out Lim_Priv) is
         begin
            Y := X;
         end Assign;

      end Pack;

      procedure Proc1 (X : in out Pack.Priv;
         Y : in     Pack.Priv := Pack.Priv_Const_In; Z : out Pack.Priv;
         U               :        Pack.Priv)
      is
      begin

         if X /= Pack.Priv_Const_In or Y /= Pack.Priv_Const_In or
           U /= Pack.Priv_Const_In then
            Failed ("WRONG INPUT VALUES  -  PROC1");
         end if;

         X := Pack.Priv_Const_Out;
         Z := Pack.Priv_Const_Out;

      end Proc1;

      procedure Proc2 (X : in out Lim_Priv; Y : in Lim_Priv;
         Z               : in out Lim_Priv; U : Lim_Priv)
      is
      begin

         if not (Equals (X, Packaged (17))) or
           not (Equals (Y, Packaged (17))) or not (Equals (U, Packaged (17)))
         then
            Failed ("WRONG INPUT VALUES  -  PROC2");
         end if;

         Assign (Packaged (13), X);
         Assign (Packaged (13), Z);

      end Proc2;

      function Func1 (Y : in Priv := Priv_Const_In; U : Priv) return Priv is
      begin

         if Y /= Priv_Const_In or U /= Priv_Const_In then
            Failed ("WRONG INPUT VALUES  -  FUNC1");
         end if;

         return Priv_Const_Out;

      end Func1;

      function Func2 (Y : in Lim_Priv; U : Lim_Priv) return Lim_Priv is
      begin

         if not (Equals (Y, Packaged (17))) or not (Equals (U, Packaged (17)))
         then
            Failed ("WRONG INPUT VALUES  -  FUNC2");
         end if;

         return Packaged (13);

      end Func2;

   begin

      --------------------------------------------------------------

      Priv_Var_1 := Priv_Const_In;
      Priv_Var_2 := Priv_Const_In;

      Proc1 (Priv_Var_1, Z => Priv_Var_2, U => Priv_Const_In);

      if Priv_Var_1 /= Pack.Priv_Const_Out or Priv_Var_2 /= Pack.Priv_Const_Out
      then
         Failed ("WRONG OUTPUT VALUES  -  PROC1");
      end if;

      --------------------------------------------------------------

      Assign (Packaged (17), Lim_Priv_Var_1);
      Assign (Packaged (17), Lim_Priv_Var_2);

      Proc2 (Lim_Priv_Var_1, Packaged (17), Lim_Priv_Var_2, Packaged (17));

      if not (Equals (Lim_Priv_Var_1, Packaged (13))) or
        not (Equals (Lim_Priv_Var_2, Packaged (13))) then
         Failed ("WRONG OUTPUT VALUES  -  PROC2");
      end if;

      --------------------------------------------------------------

      Priv_Var_1 := Priv_Const_In;
      Priv_Var_2 := Priv_Const_In;

      Priv_Var_1 := Func1 (Priv_Var_1, U => Priv_Const_In);

      if Priv_Var_1 /= Pack.Priv_Const_Out then
         Failed ("WRONG OUTPUT VALUES  -  FUNC1");
      end if;

      --------------------------------------------------------------

      Assign (Packaged (17), Lim_Priv_Var_1);
      Assign (Packaged (17), Lim_Priv_Var_2);

      Assign (Func2 (Lim_Priv_Var_1, Packaged (17)), Lim_Priv_Var_1);

      if not (Equals (Lim_Priv_Var_1, Packaged (13))) then
         Failed ("WRONG OUTPUT VALUES  -  FUNC2");
      end if;

      --------------------------------------------------------------

   end;

   Result;

end C74209a;
