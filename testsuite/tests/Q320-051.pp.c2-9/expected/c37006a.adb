-- C37006A.ADA

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
-- FOR A COMPONENT OF A RECORD, ACCESS, OR PRIVATE TYPE, OR FOR A LIMITED
-- PRIVATE COMPONENT, CHECK THAT A NON-STATIC EXPRESSION CAN BE USED IN
-- A DISCRIMINANT CONSTRAINT OR (EXCEPTING LIMITED PRIVATE COMPONENTS)
-- IN SPECIFYING A DEFAULT INITIAL VALUE.

-- R.WILLIAMS 8/28/86

with Report; use Report;
procedure C37006a is

   subtype Int is Integer range 0 .. 100;

   type Arr is array (Int range <>) of Integer;

   type Rec1 (D1, D2 : Int) is record
      A : Arr (D1 .. D2);
   end record;

   type Rec1_Name is access Rec1;

   procedure Check (Ar : Arr; Str : String) is
   begin
      if Ar'First /= 1 or Ar'Last /= 2 then
         Failed
           ("INCORRECT BOUNDS FOR R.COMP.A IN COMPONENT " &
            "OF " &
            Str &
            " TYPE");
      elsif Ar /= (3, 4) then
         Failed
           ("INITIALIZATION OF R.COMP.A IN COMPONENT OF " &
            Str &
            " TYPE FAILED");
      end if;
   end Check;

   package Pack is
      type Priv (D1, D2 : Int) is private;
      type Lim (D1, D2 : Int) is limited private;
      function Priv_Fun (Parm1, Parm2 : Integer) return Priv;
      procedure Priv_Check (R : Priv);
      procedure Lim_Check (R : Lim);

   private
      type Priv (D1, D2 : Int) is record
         A : Arr (D1 .. D2);
      end record;

      type Lim (D1, D2 : Int) is record
         A : Arr (D1 .. D2);
      end record;
   end Pack;

   package body Pack is

      function Priv_Fun (Parm1, Parm2 : Integer) return Priv is
      begin
         return (Ident_Int (1), Ident_Int (2), Arr'(1 => 3, 2 => 4));
      end Priv_Fun;

      procedure Priv_Check (R : Priv) is
      begin
         Check (R.A, "PRIVATE TYPE");
      end Priv_Check;

      procedure Lim_Check (R : Lim) is
      begin
         if R.A'First /= 1 or R.A'Last /= 2 then
            Failed
              ("INCORRECT BOUNDS FOR R.COMP.A IN " &
               "COMPONENT OF LIMITED PRIVATE TYPE");
         end if;
      end Lim_Check;
   end Pack;

   use Pack;

begin

   Test
     ("C37006A",
      "FOR A COMPONENT OF A RECORD, ACCESS, " &
      "OR PRIVATE TYPE, OR FOR A LIMITED PRIVATE " &
      "COMPONENT, CHECK THAT A NON-STATIC " &
      "EXPRESSION CAN BE USED IN A DISCRIMINANT " &
      "CONSTRAINT OR (EXCEPTING LIMITED PRIVATE " &
      "COMPONENTS) IN SPECIFYING A DEFAULT " &
      "INITIAL VALUE");

   begin
      declare

         type Rec2 is record
            Comp : Rec1 (Ident_Int (1), Ident_Int (2)) :=
              (Ident_Int (1), Ident_Int (2), Arr'(1 => 3, 2 => 4));
         end record;

         R : Rec2;

      begin
         if R.Comp.D1 = 1 and R.Comp.D2 = 2 then
            Check (R.Comp.A, "RECORD");
         else
            Failed
              ("INCORRECT VALUE FOR DISCRIMINANTS " &
               "OF RECORD TYPE COMPONENT");
         end if;

      exception
         when Constraint_Error =>
            Failed
              ("CONSTRAINT_ERROR RAISED IN STATEMENT " &
               "SEQUENCE FOLLOWING DECLARATION OF " &
               "RECORD TYPE COMPONENT");
         when others =>
            Failed
              ("OTHER EXCEPTION RAISED IN STATEMENT " &
               "SEQUENCE FOLLOWING DECLARATION OF " &
               "RECORD TYPE COMPONENT");
      end;

   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED BY DECLARATION " &
            "OF RECORD TYPE COMPONENT");
      when others =>
         Failed
           ("OTHER EXCEPTION RAISED BY DECLARATION " &
            "OF RECORD TYPE COMPONENT");
   end;

   begin
      declare

         type Rec2 is record
            Comp : Rec1_Name (Ident_Int (1), Ident_Int (2)) :=
              new Rec1'(Ident_Int (1), Ident_Int (2), Arr'(1 => 3, 2 => 4));
         end record;

         R : Rec2;

      begin
         if R.Comp.D1 = 1 and R.Comp.D2 = 2 then
            Check (R.Comp.A, "ACCESS");
         else
            Failed
              ("INCORRECT VALUE FOR DISCRIMINANTS " &
               "OF ACCESS TYPE COMPONENT");
         end if;

      exception
         when Constraint_Error =>
            Failed
              ("CONSTRAINT_ERROR RAISED IN STATEMENT " &
               "SEQUENCE FOLLOWING DECLARATION OF " &
               "ACCESS TYPE COMPONENT");
         when others =>
            Failed
              ("OTHER EXCEPTION RAISED IN STATEMENT " &
               "SEQUENCE FOLLOWING DECLARATION OF " &
               "ACCESS TYPE COMPONENT");
      end;

   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED BY DECLARATION " &
            "OF ACCESS TYPE COMPONENT");
      when others =>
         Failed
           ("OTHER EXCEPTION RAISED BY DECLARATION " &
            "OF ACCESS TYPE COMPONENT");
   end;

   begin
      declare

         type Rec2 is record
            Comp : Priv (Ident_Int (1), Ident_Int (2)) :=
              Priv_Fun (Ident_Int (1), Ident_Int (2));
         end record;

         R : Rec2;

      begin
         if R.Comp.D1 = 1 and R.Comp.D2 = 2 then
            Priv_Check (R.Comp);
         else
            Failed
              ("INCORRECT VALUE FOR DISCRIMINANTS " &
               "OF PRIVATE TYPE COMPONENT");
         end if;

      exception
         when Constraint_Error =>
            Failed
              ("CONSTRAINT_ERROR RAISED IN STATEMENT " &
               "SEQUENCE FOLLOWING DECLARATION OF " &
               "PRIVATE TYPE COMPONENT");
         when others =>
            Failed
              ("OTHER EXCEPTION RAISED IN STATEMENT " &
               "SEQUENCE FOLLOWING DECLARATION OF " &
               "PRIVATE TYPE COMPONENT");
      end;

   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED BY DECLARATION " &
            "OF PRIVATE TYPE COMPONENT");
      when others =>
         Failed
           ("OTHER EXCEPTION RAISED BY DECLARATION " &
            "OF PRIVATE TYPE COMPONENT");
   end;

   begin
      declare

         type Rec2 is record
            Comp : Lim (Ident_Int (1), Ident_Int (2));
         end record;

         R : Rec2;

      begin
         if R.Comp.D1 = 1 and R.Comp.D2 = 2 then
            Lim_Check (R.Comp);
         else
            Failed
              ("INCORRECT VALUE FOR DISCRIMINANTS " &
               "OF LIM PRIV TYPE COMPONENT");
         end if;

      exception
         when Constraint_Error =>
            Failed
              ("CONSTRAINT_ERROR RAISED IN STATEMENT " &
               "SEQUENCE FOLLOWING DECLARATION OF " &
               " LIM PRIV TYPE COMPONENT");
         when others =>
            Failed
              ("OTHER EXCEPTION RAISED IN STATEMENT " &
               "SEQUENCE FOLLOWING DECLARATION OF " &
               " LIM PRIV TYPE COMPONENT");
      end;

   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR RAISED BY DECLARATION " &
            "OF  LIM PRIV TYPE COMPONENT");
      when others =>
         Failed
           ("OTHER EXCEPTION RAISED BY DECLARATION " &
            "OF  LIM PRIV TYPE COMPONENT");
   end;

   Result;

end C37006a;
