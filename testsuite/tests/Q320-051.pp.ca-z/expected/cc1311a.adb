-- CC1311A.ADA

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
--     CHECK THAT THE DEFAULT EXPRESSIONS OF THE PARAMETERS OF A FORMAL
--     SUBPROGRAM ARE USED INSTEAD OF THE DEFAULTS (IF ANY) OF THE
--     ACTUAL SUBPROGRAM PARAMETER.

-- HISTORY:
--     RJW 06/05/86  CREATED ORIGINAL TEST.
--     VCL 08/18/87  CHANGED A COUPLE OF STATIC DEFAULT EXPRESSIONS FOR
--                   FORMAL SUBPROGRAM PARAMETERS TO DYNAMIC
--                   EXPRESSIONS VIA THE USE OF THE IDENTITY FUNCTION.
--     EDWARD V. BERARD 08/13/90
--                   ADDED CHECKS FOR MULTI-DIMENSIONAL ARRAYS.

with Report;

procedure Cc1311a is

   type Numbers is (Zero, One, Two);

   Short_Start : constant := -100;
   Short_End   : constant := 100;
   type Short_Range is range Short_Start .. Short_End;

   subtype Really_Short is Short_Range range -9 .. 0;

   type Month_Type is
     (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);

   subtype First_Half is Month_Type range Jan .. Jun;

   type Day_Type is range 1 .. 31;
   type Year_Type is range 1_904 .. 2_050;
   type Date is record
      Month : Month_Type;
      Day   : Day_Type;
      Year  : Year_Type;
   end record;

   Today : Date := (Month => Aug, Day => 8, Year => 1_990);

   First_Date : Date := (Day => 6, Month => Jun, Year => 1_967);

   subtype First_Five is Character range 'A' .. 'E';

   type Three_Dimensional is
     array (Really_Short, First_Half, First_Five) of Date;

   generic

      type First_Index is (<>);
      type Second_Index is (<>);
      type Third_Index is (<>);
      type Component_Type is private;
      Default_Value : in Component_Type;
      type Cube is
        array (First_Index, Second_Index, Third_Index) of Component_Type;
      with function Fun
        (First : in Cube :=
           (Cube'Range =>
              (Cube'Range (2) => (Cube'Range (3) => Default_Value))))
         return Cube;

   procedure Proc_With_3d_Func;

   procedure Proc_With_3d_Func is

   begin  -- PROC_WITH_3D_FUNC

      if Fun /=
        Cube'
          (Cube'Range => (Cube'Range (2) => (Cube'Range (3) => Default_Value)))
      then
         Report.Failed
           ("PROBLEMS WITH THREE DIMENSIONAL " &
            "ARRAY, FUNCTION, AND PROCEDURE.");
      end if;

   end Proc_With_3d_Func;

   generic

      type First_Index is (<>);
      type Second_Index is (<>);
      type Third_Index is (<>);
      type Component_Type is private;
      Default_Value : in Component_Type;
      type Cube is
        array (First_Index, Second_Index, Third_Index) of Component_Type;
      with function Fun
        (First : in Cube :=
           (Cube'Range =>
              (Cube'Range (2) => (Cube'Range (3) => Default_Value))))
         return Cube;

   package Pkg_With_3d_Func is
   end Pkg_With_3d_Func;

   package body Pkg_With_3d_Func is
   begin  -- PKG_WITH_3D_FUNC

      Report.Test
        ("CC1311A",
         "CHECK THAT THE DEFAULT EXPRESSIONS " &
         "OF THE PARAMETERS OF A FORMAL SUBPROGRAM ARE " &
         "USED INSTEAD OF THE DEFAULTS (IF ANY) OF THE " &
         "ACTUAL SUBPROGRAM PARAMETER");

      if Fun /=
        Cube'
          (Cube'Range => (Cube'Range (2) => (Cube'Range (3) => Default_Value)))
      then
         Report.Failed
           ("PROBLEMS WITH THREE DIMENSIONAL " &
            "ARRAY, FUNCTION, AND PACKAGE.");
      end if;

   end Pkg_With_3d_Func;

   generic

      type First_Index is (<>);
      type Second_Index is (<>);
      type Third_Index is (<>);
      type Component_Type is private;
      Default_Value : in Component_Type;
      type Cube is
        array (First_Index, Second_Index, Third_Index) of Component_Type;
      with function Fun
        (First : in Cube :=
           (Cube'Range =>
              (Cube'Range (2) => (Cube'Range (3) => Default_Value))))
         return Cube;

   function Func_With_3d_Func return Boolean;

   function Func_With_3d_Func return Boolean is
   begin  -- FUNC_WITH_3D_FUNC

      return Fun =
        Cube'
          (Cube'Range =>
             (Cube'Range (2) => (Cube'Range (3) => Default_Value)));

   end Func_With_3d_Func;

   generic

      type First_Index is (<>);
      type Second_Index is (<>);
      type Third_Index is (<>);
      type Component_Type is private;
      Default_Value : in Component_Type;
      type Cube is
        array (First_Index, Second_Index, Third_Index) of Component_Type;
      with procedure Proc
        (Input : in Cube :=
           (Cube'Range =>
              (Cube'Range (2) => (Cube'Range (3) => Default_Value)));
         Output : out Cube);

   procedure Proc_With_3d_Proc;

   procedure Proc_With_3d_Proc is

      Results : Cube;

   begin  -- PROC_WITH_3D_PROC

      Proc (Output => Results);

      if Results /=
        Cube'
          (Cube'Range => (Cube'Range (2) => (Cube'Range (3) => Default_Value)))
      then
         Report.Failed
           ("PROBLEMS WITH THREE DIMENSIONAL " &
            "ARRAY, PROCEDURE, AND PROCEDURE.");
      end if;

   end Proc_With_3d_Proc;

   generic

      type First_Index is (<>);
      type Second_Index is (<>);
      type Third_Index is (<>);
      type Component_Type is private;
      Default_Value : in Component_Type;
      type Cube is
        array (First_Index, Second_Index, Third_Index) of Component_Type;
      with procedure Proc
        (Input : in Cube :=
           (Cube'Range =>
              (Cube'Range (2) => (Cube'Range (3) => Default_Value)));
         Output : out Cube);

   package Pkg_With_3d_Proc is
   end Pkg_With_3d_Proc;

   package body Pkg_With_3d_Proc is

      Results : Cube;

   begin  -- PKG_WITH_3D_PROC

      Proc (Output => Results);

      if Results /=
        Cube'
          (Cube'Range => (Cube'Range (2) => (Cube'Range (3) => Default_Value)))
      then
         Report.Failed
           ("PROBLEMS WITH THREE DIMENSIONAL " &
            "ARRAY, PROCEDURE, AND PACKAGE.");
      end if;

   end Pkg_With_3d_Proc;

   generic

      type First_Index is (<>);
      type Second_Index is (<>);
      type Third_Index is (<>);
      type Component_Type is private;
      Default_Value : in Component_Type;
      type Cube is
        array (First_Index, Second_Index, Third_Index) of Component_Type;
      with procedure Proc
        (Input : in Cube :=
           (Cube'Range =>
              (Cube'Range (2) => (Cube'Range (3) => Default_Value)));
         Output : out Cube);

   function Func_With_3d_Proc return Boolean;

   function Func_With_3d_Proc return Boolean is

      Results : Cube;

   begin  -- FUNC_WITH_3D_PROC

      Proc (Output => Results);
      return Results =
        Cube'
          (Cube'Range =>
             (Cube'Range (2) => (Cube'Range (3) => Default_Value)));

   end Func_With_3d_Proc;

   generic
      type T is (<>);
      with function F (X : T := T'Val (0)) return T;
   function Func1 return Boolean;

   function Func1 return Boolean is
   begin  -- FUNC1
      return F = T'Val (0);
   end Func1;

   generic
      type T is (<>);
      with function F (X : T := T'Val (Report.Ident_Int (0))) return T;
   package Pkg1 is
   end Pkg1;

   package body Pkg1 is
   begin  -- PKG1
      if F /= T'Val (0) then
         Report.Failed
           ("INCORRECT DEFAULT VALUE WITH " &
            "FUNCTION 'F' AND PACKAGE 'PKG1'");
      end if;
   end Pkg1;
   generic
      type T is (<>);
      with function F (X : T := T'Val (0)) return T;
   procedure Proc1;

   procedure Proc1 is
   begin  -- PROC1
      if F /= T'Val (0) then
         Report.Failed
           ("INCORRECT DEFAULT VALUE WITH " &
            "FUNCTION 'F' AND PROCEDURE 'PROC1'");
      end if;
   end Proc1;

   generic
      type T is (<>);
      with procedure P (Results : out T; X : T := T'Val (0));
   function Func2 return Boolean;

   function Func2 return Boolean is
      Results : T;
   begin  -- FUNC2
      P (Results);
      return Results = T'Val (0);
   end Func2;

   generic
      type T is (<>);
      with procedure P
        (Results : out T; X : T := T'Val (Report.Ident_Int (0)));
   package Pkg2 is
   end Pkg2;

   package body Pkg2 is
      Results : T;
   begin  -- PKG2
      P (Results);
      if Results /= T'Val (0) then
         Report.Failed
           ("INCORRECT DEFAULT VALUE WITH " &
            "PROCEDURE 'P' AND PACKAGE 'PKG2'");
      end if;
   end Pkg2;

   generic
      type T is (<>);
      with procedure P (Results : out T; X : T := T'Val (0));
   procedure Proc2;

   procedure Proc2 is
      Results : T;
   begin  -- PROC2
      P (Results);
      if Results /= T'Val (0) then
         Report.Failed
           ("INCORRECT DEFAULT VALUE WITH " &
            "PROCEDURE 'P' AND PROCEDURE 'PROC2'");
      end if;
   end Proc2;

   function F1 (A : Numbers := One) return Numbers is
   begin  -- F1
      return A;
   end F1;

   procedure P2 (Outvar : out Numbers; Invar : Numbers := Two) is
   begin  -- P2
      Outvar := Invar;
   end P2;

   function Td_Func
     (First : in Three_Dimensional :=
        (Three_Dimensional'Range =>
           (Three_Dimensional'Range (2) =>
              (Three_Dimensional'Range (3) => First_Date))))
      return Three_Dimensional
   is

   begin  -- TD_FUNC

      return First;

   end Td_Func;

   procedure Td_Proc
     (Input : in Three_Dimensional :=
        (Three_Dimensional'Range =>
           (Three_Dimensional'Range (2) =>
              (Three_Dimensional'Range (3) => First_Date)));
      Output : out Three_Dimensional)
   is
   begin  -- TD_PROC

      Output := Input;

   end Td_Proc;

   procedure New_Proc_With_3d_Func is new Proc_With_3d_Func
     (First_Index   => Really_Short, Second_Index => First_Half,
      Third_Index   => First_Five, Component_Type => Date,
      Default_Value => Today, Cube => Three_Dimensional, Fun => Td_Func);

   package New_Pkg_With_3d_Func is new Pkg_With_3d_Func
     (First_Index   => Really_Short, Second_Index => First_Half,
      Third_Index   => First_Five, Component_Type => Date,
      Default_Value => Today, Cube => Three_Dimensional, Fun => Td_Func);

   function New_Func_With_3d_Func is new Func_With_3d_Func
     (First_Index   => Really_Short, Second_Index => First_Half,
      Third_Index   => First_Five, Component_Type => Date,
      Default_Value => Today, Cube => Three_Dimensional, Fun => Td_Func);

   procedure New_Proc_With_3d_Proc is new Proc_With_3d_Proc
     (First_Index   => Really_Short, Second_Index => First_Half,
      Third_Index   => First_Five, Component_Type => Date,
      Default_Value => Today, Cube => Three_Dimensional, Proc => Td_Proc);

   package New_Pkg_With_3d_Proc is new Pkg_With_3d_Proc
     (First_Index   => Really_Short, Second_Index => First_Half,
      Third_Index   => First_Five, Component_Type => Date,
      Default_Value => Today, Cube => Three_Dimensional, Proc => Td_Proc);

   function New_Func_With_3d_Proc is new Func_With_3d_Proc
     (First_Index   => Really_Short, Second_Index => First_Half,
      Third_Index   => First_Five, Component_Type => Date,
      Default_Value => Today, Cube => Three_Dimensional, Proc => Td_Proc);

   function Nfunc1 is new Func1 (Numbers, F1);
   package Npkg1 is new Pkg1 (Numbers, F1);
   procedure Nproc1 is new Proc1 (Numbers, F1);

   function Nfunc2 is new Func2 (Numbers, P2);
   package Npkg2 is new Pkg2 (Numbers, P2);
   procedure Nproc2 is new Proc2 (Numbers, P2);

begin  -- CC1311A

   if not Nfunc1 then
      Report.Failed ("INCORRECT DEFAULT VALUE " & "WITH FUNCTION 'NFUNC1'");
   end if;

   if not Nfunc2 then
      Report.Failed ("INCORRECT DEFAULT VALUE " & "WITH FUNCTION 'NFUNC2'");
   end if;

   Nproc1;
   Nproc2;

   New_Proc_With_3d_Func;

   if not New_Func_With_3d_Func then
      Report.Failed
        ("PROBLEMS WITH THREE DIMENSIONAL ARRAY, " &
         "FUNCTION, AND FUNCTION.");
   end if;

   New_Proc_With_3d_Proc;

   if not New_Func_With_3d_Proc then
      Report.Failed
        ("PROBLEMS WITH THREE DIMENSIONAL ARRAY, " &
         "FUNCTION, AND PROCEDURE.");
   end if;

   Report.Result;

end Cc1311a;
