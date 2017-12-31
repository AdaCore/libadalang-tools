-- CC3017B.ADA

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
-- CHECK THAT AN INSTANCE OF A GENERIC PROCEDURE MUST DECLARE A PROCEDURE AND
-- THAT AN INSTANCE OF A GENERIC FUNCTION MUST DECLARE A FUNCTION. CHECK THAT
-- CONSTRAINT_ERROR IS NOT RAISED IF THE DEFAULT VALUE FOR A FORMAL PARAMETER
-- DOES NOT SATISFY THE CONSTRAINTS OF THE SUBTYPE_INDICATION WHEN THE
-- DECLARATION IS ELABORATED, ONLY WHEN THE DEFAULT IS USED.

--   SUBTESTS ARE:
--        (A) ARRAY PARAMETERS CONSTRAINED WITH NONSTATIC BOUNDS AND
--            INITIALIZED WITH A STATIC AGGREGATE.
--        (B) A SCALAR PARAMETER WITH NON-STATIC RANGE CONSTRAINTS
--            INITIALIZED WITH A STATIC VALUE.
--        (C) A RECORD PARAMETER WHOSE COMPONENTS HAVE NON-STATIC
--            CONSTRAINTS INITIALIZED WITH A STATIC AGGREGATE.
--        (D) AN ARRAY PARAMETER CONSTRAINED WITH STATIC BOUNDS ON SUB-
--            SCRIPTS AND NON-STATIC BOUNDS ON COMPONENTS, INITIALIZED
--            WITH A STATIC AGGREGATE.
--        (E) A RECORD PARAMETER WITH A NON-STATIC CONSTRAINT
--            INITIALIZED WITH A STATIC AGGREGATE.

-- EDWARD V. BERARD, 7 AUGUST 1990

with Report;

procedure Cc3017b is

begin

   Report.Test
     ("CC3017B",
      "CHECK THAT AN INSTANCE OF A GENERIC " &
      "PROCEDURE MUST DECLARE A PROCEDURE AND THAT AN " &
      "INSTANCE OF A GENERIC FUNCTION MUST DECLARE A " &
      "FUNCTION. CHECK THAT CONSTRAINT_ERROR IS NOT " &
      "RAISED IF AN INITIALIZATION VALUE DOES NOT SATISFY " &
      "CONSTRAINTS ON A FORMAL PARAMETER");

   --------------------------------------------------

   Nonstat_Array_Parms :

   declare

--        (A) ARRAY PARAMETERS CONSTRAINED WITH NONSTATIC BOUNDS AND
--            INITIALIZED WITH A STATIC AGGREGATE.

      type Number is range 1 .. 100;

      generic

         type Integer_Type is range <>;
         Lower : in Integer_Type;
         Upper : in Integer_Type;

      procedure Pa (First : in Integer_Type; Second : in Integer_Type);

      procedure Pa (First : in Integer_Type; Second : in Integer_Type) is

         type A1 is
           array
             (Integer_Type range Lower .. First,
              Integer_Type range Lower .. Second) of Integer_Type;

         procedure Pa1 (A : A1 := ((Lower, Upper), (Upper, Upper))) is
         begin
            Report.Failed ("BODY OF PA1 EXECUTED");
         exception
            when others =>
               Report.Failed ("EXCEPTION RAISED IN PA1");
         end Pa1;

      begin  -- PA
         Pa1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Report.Failed ("WRONG EXCEPTION RAISED - PA1");
      end Pa;

      procedure New_Pa is new Pa (Integer_Type => Number, Lower => 1,
         Upper                                 => 50);

   begin   -- NONSTAT_ARRAY_PARMS

      New_Pa (First => Number (25), Second => Number (75));

   exception
      when others =>
         Report.Failed ("EXCEPTION RAISED IN CALL TO NEW_PA");

   end Nonstat_Array_Parms;

   --------------------------------------------------

   Scalar_Non_Static :

   declare

--        (B) A SCALAR PARAMETER WITH NON-STATIC RANGE CONSTRAINTS
--            INITIALIZED WITH A STATIC VALUE.

      type Number is range 1 .. 100;

      generic

         type Integer_Type is range <>;
         Static_Value : in Integer_Type;

      procedure Pb (Lower : in Integer_Type; Upper : in Integer_Type);

      procedure Pb (Lower : in Integer_Type; Upper : in Integer_Type) is

         subtype Int is Integer_Type range Lower .. Upper;

         procedure Pb1 (I : Int := Static_Value) is
         begin  -- PB1
            Report.Failed ("BODY OF PB1 EXECUTED");
         exception
            when others =>
               Report.Failed ("EXCEPTION RAISED IN PB1");
         end Pb1;

      begin  -- PB
         Pb1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Report.Failed ("WRONG EXCEPTION RAISED - PB1");
      end Pb;

      procedure New_Pb is new Pb (Integer_Type => Number, Static_Value => 20);

   begin   -- SCALAR_NON_STATIC

      New_Pb (Lower => Number (25), Upper => Number (75));

   exception
      when others =>
         Report.Failed ("EXCEPTION RAISED IN CALL TO NEW_PB");
   end Scalar_Non_Static;

   --------------------------------------------------

   Rec_Non_Stat_Comps :

   declare

--        (C) A RECORD PARAMETER WHOSE COMPONENTS HAVE NON-STATIC
--            CONSTRAINTS INITIALIZED WITH A STATIC AGGREGATE.

      type Number is range 1 .. 100;

      generic

         type Integer_Type is range <>;
         F_Static_Value : in Integer_Type;
         S_Static_Value : in Integer_Type;
         T_Static_Value : in Integer_Type;
         L_Static_Value : in Integer_Type;

      procedure Pc (Lower : in Integer_Type; Upper : in Integer_Type);

      procedure Pc (Lower : in Integer_Type; Upper : in Integer_Type) is

         subtype Subinteger_Type is Integer_Type range Lower .. Upper;
         type Ar1 is array (Integer range 1 .. 3) of Subinteger_Type;
         type Rec is record
            First  : Subinteger_Type;
            Second : Ar1;
         end record;

         procedure Pc1
           (R : Rec :=
              (F_Static_Value,
               (S_Static_Value, T_Static_Value, L_Static_Value)))
         is
         begin  -- PC1
            Report.Failed ("BODY OF PC1 EXECUTED");
         exception
            when others =>
               Report.Failed ("EXCEPTION RAISED IN PC1");
         end Pc1;

      begin  -- PC
         Pc1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Report.Failed ("WRONG EXCEPTION RAISED - PC1");
      end Pc;

      procedure New_Pc is new Pc (Integer_Type => Number, F_Static_Value => 15,
         S_Static_Value => 19, T_Static_Value => 85, L_Static_Value => 99);

   begin   -- REC_NON_STAT_COMPS
      New_Pc (Lower => 20, Upper => 80);
   exception
      when others =>
         Report.Failed ("EXCEPTION RAISED IN CALL TO NEW_PC");
   end Rec_Non_Stat_Comps;

   --------------------------------------------------

   First_Static_Array :

   declare

--        (D) AN ARRAY PARAMETER CONSTRAINED WITH STATIC BOUNDS ON SUB-
--            SCRIPTS AND NON-STATIC BOUNDS ON COMPONENTS, INITIALIZED
--            WITH A STATIC AGGREGATE.

      type Number is range 1 .. 100;

      generic

         type Integer_Type is range <>;
         F_Static_Value : in Integer_Type;
         S_Static_Value : in Integer_Type;
         T_Static_Value : in Integer_Type;
         L_Static_Value : in Integer_Type;
         A_Static_Value : in Integer_Type;
         B_Static_Value : in Integer_Type;
         C_Static_Value : in Integer_Type;
         D_Static_Value : in Integer_Type;

      procedure P1d (Lower : in Integer_Type; Upper : in Integer_Type);

      procedure P1d (Lower : in Integer_Type; Upper : in Integer_Type) is

         subtype Subinteger_Type is Integer_Type range Lower .. Upper;

         type A1 is
           array
             (Integer_Type range F_Static_Value .. S_Static_Value,
              Integer_Type range T_Static_Value ..
                  L_Static_Value) of Subinteger_Type;

         procedure P1d1
           (A : A1 :=
              ((A_Static_Value, B_Static_Value),
               (C_Static_Value, D_Static_Value)))
         is
         begin  -- P1D1
            Report.Failed ("BODY OF P1D1 EXECUTED");
         exception
            when others =>
               Report.Failed ("EXCEPTION RAISED IN P1D1");
         end P1d1;

      begin  -- P1D
         P1d1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Report.Failed ("WRONG EXCEPTION RAISED - P1D1");
      end P1d;

      procedure New_P1d is new P1d (Integer_Type => Number,
         F_Static_Value => 21, S_Static_Value => 37, T_Static_Value => 67,
         L_Static_Value => 79, A_Static_Value => 11, B_Static_Value => 88,
         C_Static_Value                          => 87, D_Static_Value => 13);

   begin  -- FIRST_STATIC_ARRAY
      New_P1d (Lower => 10, Upper => 90);
   exception
      when others =>
         Report.Failed ("EXCEPTION RAISED IN CALL TO NEW_P1D");
   end First_Static_Array;

   --------------------------------------------------

   Second_Static_Array :

   declare

--        (D) AN ARRAY PARAMETER CONSTRAINED WITH STATIC BOUNDS ON SUB-
--            SCRIPTS AND NON-STATIC BOUNDS ON COMPONENTS, INITIALIZED
--            WITH A STATIC AGGREGATE.

      type Number is range 1 .. 100;

      generic

         type Integer_Type is range <>;
         F_Static_Value : in Integer_Type;
         S_Static_Value : in Integer_Type;
         T_Static_Value : in Integer_Type;
         L_Static_Value : in Integer_Type;
         A_Static_Value : in Integer_Type;
         B_Static_Value : in Integer_Type;

      procedure P2d (Lower : in Integer_Type; Upper : in Integer_Type);

      procedure P2d (Lower : in Integer_Type; Upper : in Integer_Type) is

         subtype Subinteger_Type is Integer_Type range Lower .. Upper;

         type A1 is
           array
             (Integer_Type range F_Static_Value .. S_Static_Value,
              Integer_Type range T_Static_Value ..
                  L_Static_Value) of Subinteger_Type;

         procedure P2d1
           (A : A1 :=
              (F_Static_Value .. S_Static_Value =>
                 (A_Static_Value, B_Static_Value)))
         is
         begin  -- P2D1
            Report.Failed ("BODY OF P2D1 EXECUTED");
         exception
            when others =>
               Report.Failed ("EXCEPTION RAISED IN P2D1");
         end P2d1;

      begin  -- P2D
         P2d1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Report.Failed ("WRONG EXCEPTION RAISED - P2D1");
      end P2d;

      procedure New_P2d is new P2d (Integer_Type => Number,
         F_Static_Value => 21, S_Static_Value => 37, T_Static_Value => 67,
         L_Static_Value => 79, A_Static_Value => 7, B_Static_Value => 93);

   begin  -- SECOND_STATIC_ARRAY
      New_P2d (Lower => 5, Upper => 95);
   exception
      when others =>
         Report.Failed ("EXCEPTION RAISED IN CALL TO NEW_P2D");
   end Second_Static_Array;

   --------------------------------------------------

   Rec_Non_Static_Cons :

   declare

--        (E) A RECORD PARAMETER WITH A NON-STATIC CONSTRAINT
--            INITIALIZED WITH A STATIC AGGREGATE.

      type Number is range 1 .. 100;

      generic

         type Integer_Type is range <>;
         F_Static_Value : in Integer_Type;
         S_Static_Value : in Integer_Type;
         T_Static_Value : in Integer_Type;
         L_Static_Value : in Integer_Type;
         D_Static_Value : in Integer_Type;

      procedure Pe (Lower : in Integer_Type; Upper : in Integer_Type);

      procedure Pe (Lower : in Integer_Type; Upper : in Integer_Type) is

         subtype Subinteger_Type is Integer_Type range Lower .. Upper;
         type Ar1 is array (Integer range 1 .. 3) of Subinteger_Type;

         type Rec (Discrim : Subinteger_Type) is record
            First  : Subinteger_Type;
            Second : Ar1;
         end record;

         subtype Rec4 is Rec (Lower);

         procedure Pe1
           (R : Rec4 :=
              (D_Static_Value, F_Static_Value,
               (S_Static_Value, T_Static_Value, L_Static_Value)))
         is
         begin  -- PE1
            Report.Failed ("BODY OF PE1 EXECUTED");
         exception
            when others =>
               Report.Failed ("EXCEPTION RAISED IN PE1");
         end Pe1;

      begin  -- PE
         Pe1;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Report.Failed ("WRONG EXCEPTION RAISED - PE1");
      end Pe;

      procedure New_Pe is new Pe (Integer_Type => Number, F_Static_Value => 37,
         S_Static_Value => 21, T_Static_Value => 67, L_Static_Value => 79,
         D_Static_Value                        => 44);

   begin  -- REC_NON_STATIC_CONS
      New_Pe (Lower => 2, Upper => 99);
   exception
      when others =>
         Report.Failed ("EXCEPTION RAISED IN CALL TO NEW_PE");
   end Rec_Non_Static_Cons;

   --------------------------------------------------

   Report.Result;

end Cc3017b;
