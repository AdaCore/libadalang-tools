-- CXA5012.A
--
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
--
-- OBJECTIVE:
--      Check that, for both Float_Random and Discrete_Random packages,
--      the following are true:
--      1) the procedures Save and Reset can be used to save the
--         specific state of a random number generator, and then restore
--         the specific state to the generator following some intermediate
--         generator activity.
--      2) the Function Image can be used to obtain a string
--         representation of the state of a generator; and that the
--         Function Value will transform a string representation of the
--         state of a random number generator into the actual state object.
--      3) a call to Function Value, with a string value that is
--         not the image of any generator state, is a bounded error. This
--         error either raises Constraint_Error or Program_Error, or is
--         accepted. (See Technical Corrigendum 1).
--
-- TEST DESCRIPTION:
--      This test evaluates components of the Ada.Numerics.Float_Random and
--      Ada.Numerics.Discrete_Random packages.
--      The first objective block of this test uses Procedure Save to
--      save the particular state of a random number generator.  The random
--      number generator then generates a series of random numbers.  The
--      saved state variable is then used to reset (using Procedure Reset)
--      the generator back to the state it was in at the point of the call
--      to Save.  Random values are then generated from this restored
--      generator, and compared with expected values.
--      The second objective block of this test uses Function Image to
--      provide a string representation of a state code.  This string is
--      then transformed back to a state code value, and used to reset a
--      random number generator to the saved state.  Random values are
--      likewise generated from this restored generator, and compared with
--      expected values.
--
--
-- CHANGE HISTORY:
--      25 Apr 95   SAIC    Initial prerelease version.
--      17 Jul 95   SAIC    Incorporated reviewer comments.
--      17 Dec 97   EDS     Change subtype upper limit from 100_000 to 10_000.
--      16 Sep 99   RLB     Updated objective 3 for Technical Corrigendum 1
--                          changes.

--!

with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Bounded;
with Impdef;
with Report;

procedure Cxa5012 is

begin

   Report.Test
     ("CXA5012",
      "Check the effect of Procedures Save and " &
      "Reset, and Functions Image and Value " &
      "from the Ada.Numerics.Discrete_Random " &
      "and Float_Random packages");

   Test_Block : declare

      use Ada.Numerics, Ada.Strings.Bounded;

      -- Declare an integer subtype and an enumeration subtype, and use them
      -- to instantiate the discrete random number generator generic package.

      subtype Discrete_Range is Integer range 1 .. 10_000;
      type Suit_Of_Cards is
        (Ace,
         One,
         Two,
         Three,
         Four,
         Five,
         Six,
         Seven,
         Eight,
         Nine,
         Ten,
         Jack,
         Queen,
         King);
      package Discrete_Pack is new Discrete_Random (Discrete_Range);
      package Card_Pack is new Discrete_Random (Suit_Of_Cards);

      -- Declaration of random number generator objects.

      Dgen_1, Dgen_2 : Discrete_Pack.Generator;
      Egen_1, Egen_2 : Card_Pack.Generator;
      Fgen_1, Fgen_2 : Float_Random.Generator;

      -- Variables declared to hold random numbers over the inclusive range
      -- of their corresponding type.

      Dval_1, Dval_2 : Discrete_Range;
      Eval_1, Eval_2 : Suit_Of_Cards;
      Fval_1, Fval_2 : Float_Random.Uniformly_Distributed;

      -- Declaration of State variables used to hold the state of the
      -- random number generators.

      Dstate_1, Dstate_2 : Discrete_Pack.State;
      Estate_1, Estate_2 : Card_Pack.State;
      Fstate_1, Fstate_2 : Float_Random.State;

      -- Declaration of bounded string packages instantiated with the
      -- value of Max_Image_Width constant, and bounded string variables
      -- used to hold the image of random number generator states.

      package Dstring_Pack is new Generic_Bounded_Length
        (Discrete_Pack.Max_Image_Width);
      package Estring_Pack is new Generic_Bounded_Length
        (Card_Pack.Max_Image_Width);
      package Fstring_Pack is new Generic_Bounded_Length
        (Float_Random.Max_Image_Width);

      use Dstring_Pack, Estring_Pack, Fstring_Pack;

      Dstring_1,
      Dstring_2 : Dstring_Pack.Bounded_String :=
        Dstring_Pack.Null_Bounded_String;
      Estring_1,
      Estring_2 : Estring_Pack.Bounded_String :=
        Estring_Pack.Null_Bounded_String;
      Fstring_1,
      Fstring_2 : Fstring_Pack.Bounded_String :=
        Fstring_Pack.Null_Bounded_String;

      -- Test variables.

      Tc_Count : Natural;
      Tc_Discrete_Check_Failed,
      Tc_Enum_Check_Failed,
      Tc_Float_Check_Failed : Boolean :=
        False;
      Tc_Seed : Integer;

   begin

      Objective_1 :
         -- Check that the procedures Save and Reset can be used to save the
         -- specific state of a random number generator, and then restore the
         -- specific state to the generator following some intermediate
         -- generator activity.
          declare

         First_Row     : constant := 1;
         Second_Row    : constant := 2;
         Tc_Max_Values : constant := 100;

         Tc_Discrete_Array : array
           (First_Row .. Second_Row, 1 .. Tc_Max_Values) of Discrete_Range;
         Tc_Enum_Array : array
           (First_Row .. Second_Row, 1 .. Tc_Max_Values) of Suit_Of_Cards;
         Tc_Float_Array : array
           (First_Row ..
                Second_Row,
              1 ..
                Tc_Max_Values) of Float_Random.Uniformly_Distributed;
      begin

         -- The state of the random number generators are saved to state
         -- variables using the procedure Save.

         Discrete_Pack.Save (Gen => Dgen_1, To_State => Dstate_1);
         Card_Pack.Save (Gen => Egen_1, To_State => Estate_1);
         Float_Random.Save (Gen => Fgen_1, To_State => Fstate_1);

         -- Random number generators are used to fill the first half of the
         -- first row of the arrays with randomly generated values.

         for I in 1 .. Tc_Max_Values / 2 loop
            Tc_Discrete_Array (First_Row, I) := Discrete_Pack.Random (Dgen_1);
            Tc_Enum_Array (First_Row, I)     := Card_Pack.Random (Egen_1);
            Tc_Float_Array (First_Row, I)    := Float_Random.Random (Fgen_1);
         end loop;

         -- The random number generators are reset to the states saved in the
         -- state variables, using the procedure Reset.

         Discrete_Pack.Reset (Gen => Dgen_1, From_State => Dstate_1);
         Card_Pack.Reset (Gen => Egen_1, From_State => Estate_1);
         Float_Random.Reset (Gen => Fgen_1, From_State => Fstate_1);

         -- The same random number generators are used to fill the first half
         -- of the second row of the arrays with randomly generated values.

         for I in 1 .. Tc_Max_Values / 2 loop
            Tc_Discrete_Array (Second_Row, I) := Discrete_Pack.Random (Dgen_1);
            Tc_Enum_Array (Second_Row, I)     := Card_Pack.Random (Egen_1);
            Tc_Float_Array (Second_Row, I)    := Float_Random.Random (Fgen_1);
         end loop;

         -- Run the random number generators many times (not using results).

         for I in Discrete_Range'Range loop
            Dval_1 := Discrete_Pack.Random (Dgen_1);
            Eval_1 := Card_Pack.Random (Egen_1);
            Fval_1 := Float_Random.Random (Fgen_1);
         end loop;

         -- The states of the random number generators are saved to state
         -- variables using the procedure Save.

         Discrete_Pack.Save (Gen => Dgen_1, To_State => Dstate_1);
         Card_Pack.Save (Gen => Egen_1, To_State => Estate_1);
         Float_Random.Save (Gen => Fgen_1, To_State => Fstate_1);

         -- The last half of the first row of the arrays are filled with
         -- values generated from the same random number generators.

         for I in (Tc_Max_Values / 2 + 1) .. Tc_Max_Values loop
            Tc_Discrete_Array (First_Row, I) := Discrete_Pack.Random (Dgen_1);
            Tc_Enum_Array (First_Row, I)     := Card_Pack.Random (Egen_1);
            Tc_Float_Array (First_Row, I)    := Float_Random.Random (Fgen_1);
         end loop;

         -- The random number generators are reset to the states saved in the
         -- state variables, using the procedure Reset.

         Discrete_Pack.Reset (Gen => Dgen_1, From_State => Dstate_1);
         Card_Pack.Reset (Gen => Egen_1, From_State => Estate_1);
         Float_Random.Reset (Gen => Fgen_1, From_State => Fstate_1);

         -- The last half of the second row of the arrays are filled with
         -- values generated from the same random number generator.
         -- These values should exactly mirror the values in the last half
         -- of the first row of the arrays that had been previously generated.

         for I in (Tc_Max_Values / 2 + 1) .. Tc_Max_Values loop
            Tc_Discrete_Array (Second_Row, I) := Discrete_Pack.Random (Dgen_1);
            Tc_Enum_Array (Second_Row, I)     := Card_Pack.Random (Egen_1);
            Tc_Float_Array (Second_Row, I)    := Float_Random.Random (Fgen_1);
         end loop;

         -- Check that the values in the two rows of the arrays are identical.

         for I in 1 .. Tc_Max_Values loop
            if Tc_Discrete_Array (First_Row, I) /=
              Tc_Discrete_Array (Second_Row, I)
            then
               Tc_Discrete_Check_Failed := True;
               exit;
            end if;
         end loop;

         for I in 1 .. Tc_Max_Values loop
            if Tc_Enum_Array (First_Row, I) /=
              Tc_Enum_Array (Second_Row, I)
            then
               Tc_Enum_Check_Failed := True;
               exit;
            end if;
         end loop;

         for I in 1 .. Tc_Max_Values loop
            if Tc_Float_Array (First_Row, I) /=
              Tc_Float_Array (Second_Row, I)
            then
               Tc_Float_Check_Failed := True;
               exit;
            end if;
         end loop;

         if Tc_Discrete_Check_Failed then
            Report.Failed
              ("Discrete random values generated following use " &
               "of procedures Save and Reset were not the same");
            Tc_Discrete_Check_Failed := False;
         end if;

         if Tc_Enum_Check_Failed then
            Report.Failed
              ("Enumeration random values generated following " &
               "use of procedures Save and Reset were not the " &
               "same");
            Tc_Enum_Check_Failed := False;
         end if;

         if Tc_Float_Check_Failed then
            Report.Failed
              ("Float random values generated following use " &
               "of procedures Save and Reset were not the same");
            Tc_Float_Check_Failed := False;
         end if;

      end Objective_1;

      Objective_2 :
         -- Check that the Function Image can be used to obtain a string
         -- representation of the state of a generator.
         -- Check that the Function Value will transform a string
         -- representation of the state of a random number generator
         -- into the actual state object.
          begin

         -- Use two discrete and float random number generators to generate
         -- a series of values (so that the generators are no longer in their
         -- initial states, and they have generated the same number of
         -- random values).

         Tc_Seed := Integer (Discrete_Pack.Random (Dgen_1));
         Discrete_Pack.Reset (Dgen_1, Tc_Seed);
         Discrete_Pack.Reset (Dgen_2, Tc_Seed);
         Card_Pack.Reset (Egen_1, Tc_Seed);
         Card_Pack.Reset (Egen_2, Tc_Seed);
         Float_Random.Reset (Fgen_1, Tc_Seed);
         Float_Random.Reset (Fgen_2, Tc_Seed);

         for I in 1 .. 1_000 loop
            Dval_1 := Discrete_Pack.Random (Dgen_1);
            Dval_2 := Discrete_Pack.Random (Dgen_2);
            Eval_1 := Card_Pack.Random (Egen_1);
            Eval_2 := Card_Pack.Random (Egen_2);
            Fval_1 := Float_Random.Random (Fgen_1);
            Fval_2 := Float_Random.Random (Fgen_2);
         end loop;

         -- Use the Procedure Save to save the states of the generators
         -- to state variables.

         Discrete_Pack.Save (Gen => Dgen_1, To_State => Dstate_1);
         Discrete_Pack.Save (Dgen_2, To_State => Dstate_2);
         Card_Pack.Save (Gen => Egen_1, To_State => Estate_1);
         Card_Pack.Save (Egen_2, To_State => Estate_2);
         Float_Random.Save (Fgen_1, To_State => Fstate_1);
         Float_Random.Save (Fgen_2, Fstate_2);

         -- Use the Function Image to produce a representation of the state
         -- codes as (bounded) string objects.

         Dstring_1 :=
           Dstring_Pack.To_Bounded_String
             (Discrete_Pack.Image (Of_State => Dstate_1));
         Dstring_2 :=
           Dstring_Pack.To_Bounded_String (Discrete_Pack.Image (Dstate_2));
         Estring_1 :=
           Estring_Pack.To_Bounded_String
             (Card_Pack.Image (Of_State => Estate_1));
         Estring_2 :=
           Estring_Pack.To_Bounded_String (Card_Pack.Image (Estate_2));
         Fstring_1 :=
           Fstring_Pack.To_Bounded_String
             (Float_Random.Image (Of_State => Fstate_1));
         Fstring_2 :=
           Fstring_Pack.To_Bounded_String (Float_Random.Image (Fstate_2));

         -- Compare the bounded string objects for equality.

         if Dstring_1 /= Dstring_2 then
            Report.Failed
              ("String values returned from Function Image " &
               "depict different states of Discrete generators");
         end if;
         if Estring_1 /= Estring_2 then
            Report.Failed
              ("String values returned from Function Image " &
               "depict different states of Enumeration " &
               "generators");
         end if;
         if Fstring_1 /= Fstring_2 then
            Report.Failed
              ("String values returned from Function Image " &
               "depict different states of Float generators");
         end if;

         -- The string representation of a state code is transformed back
         -- to a state code variable using the Function Value.

         Dstate_1 :=
           Discrete_Pack.Value
             (Coded_State => Dstring_Pack.To_String (Dstring_1));
         Estate_1 := Card_Pack.Value (Estring_Pack.To_String (Estring_1));
         Fstate_1 := Float_Random.Value (Fstring_Pack.To_String (Fstring_1));

         -- One of the (pair of each type of ) generators is used to generate
         -- a series of random values, getting them "out of synch" with the
         -- specific generation sequence of the other generators.

         for I in 1 .. 100 loop
            Dval_1 := Discrete_Pack.Random (Dgen_1);
            Eval_1 := Card_Pack.Random (Egen_1);
            Fval_1 := Float_Random.Random (Fgen_1);
         end loop;

         -- The "out of synch" generators are reset to the previous state they
         -- had when their states were saved, and they should now have the same
         -- states as the generators that did not generate the values above.

         Discrete_Pack.Reset (Gen => Dgen_1, From_State => Dstate_1);
         Card_Pack.Reset (Gen => Egen_1, From_State => Estate_1);
         Float_Random.Reset (Gen => Fgen_1, From_State => Fstate_1);

         -- All generators should now be in the same state, so the
         -- random values they produce should be the same.

         for I in 1 .. 1_000 loop
            if Discrete_Pack.Random (Dgen_1) /=
              Discrete_Pack.Random (Dgen_2)
            then
               Tc_Discrete_Check_Failed := True;
               exit;
            end if;
         end loop;

         for I in 1 .. 1_000 loop
            if Card_Pack.Random (Egen_1) /= Card_Pack.Random (Egen_2) then
               Tc_Enum_Check_Failed := True;
               exit;
            end if;
         end loop;

         for I in 1 .. 1_000 loop
            if Float_Random.Random (Fgen_1) /=
              Float_Random.Random (Fgen_2)
            then
               Tc_Float_Check_Failed := True;
               exit;
            end if;
         end loop;

         if Tc_Discrete_Check_Failed then
            Report.Failed
              ("Random values generated following use of " &
               "procedures Image and Value were not the same " &
               "for Discrete generator");
         end if;
         if Tc_Enum_Check_Failed then
            Report.Failed
              ("Random values generated following use of " &
               "procedures Image and Value were not the same " &
               "for Enumeration generator");
         end if;
         if Tc_Float_Check_Failed then
            Report.Failed
              ("Random values generated following use of " &
               "procedures Image and Value were not the same " &
               "for Float generator");
         end if;

      end Objective_2;

      Objective_3 :
         -- Check that a call to Function Value, with a string value that is
         -- not the image of any generator state, is a bounded error. This
         -- error either raises Constraint_Error or Program_Error, or is
         -- accepted. (See Technical Corrigendum 1).
          declare
         Not_A_State : constant String := Impdef.Non_State_String;
      begin

         begin
            Dstate_1 := Discrete_Pack.Value (Not_A_State);
            if Not_A_State /= "**NONE**" then
               Report.Failed
                 ("Exception not raised by Function " &
                  "Ada.Numerics.Discrete_Random.Value when " &
                  "provided a string input that does not " &
                  "represent the state of a random number " &
                  "generator");
            else
               Report.Comment
                 ("All strings represent states for Function " &
                  "Ada.Numerics.Discrete_Random.Value");
            end if;
            Discrete_Pack.Reset (Dgen_1, Dstate_1);
         exception
            when Constraint_Error =>
               null;  -- OK, expected exception.
               Report.Comment
                 ("Constraint_Error raised by Function " &
                  "Ada.Numerics.Discrete_Random.Value when " &
                  "provided a string input that does not " &
                  "represent the state of a random number " &
                  "generator");
            when Program_Error => -- OK, expected exception.
               Report.Comment
                 ("Program_Error raised by Function " &
                  "Ada.Numerics.Discrete_Random.Value when " &
                  "provided a string input that does not " &
                  "represent the state of a random number " &
                  "generator");
            when others =>
               Report.Failed
                 ("Unexpected exception raised by Function " &
                  "Ada.Numerics.Discrete_Random.Value when " &
                  "provided a string input that does not " &
                  "represent the state of a random number " &
                  "generator");
         end;

         begin
            Estate_1 := Card_Pack.Value (Not_A_State);
            if Not_A_State /= "**NONE**" then
               Report.Failed
                 ("Exception not raised by Function " &
                  "Ada.Numerics.Discrete_Random.Value when " &
                  "provided a string input that does not " &
                  "represent the state of an enumeration " &
                  "random number generator");
            else
               Report.Comment
                 ("All strings represent states for Function " &
                  "Ada.Numerics.Discrete_Random.Value");
            end if;
            Card_Pack.Reset (Egen_1, Estate_1);
         exception
            when Constraint_Error =>
               null;  -- OK, expected exception.
            when Program_Error =>
               null; -- OK, expected exception.
            when others =>
               Report.Failed
                 ("Unexpected exception raised by Function " &
                  "Ada.Numerics.Discrete_Random.Value when " &
                  "provided a string input that does not " &
                  "represent the state of an enumeration " &
                  "random number generator");
         end;

         begin
            Fstate_1 := Float_Random.Value (Not_A_State);
            if Not_A_State /= "**NONE**" then
               Report.Failed
                 ("Exception not raised by an " &
                  "instantiated version of " &
                  "Ada.Numerics.Float_Random.Value when " &
                  "provided a string input that does not " &
                  "represent the state of a random number " &
                  "generator");
            else
               Report.Comment
                 ("All strings represent states for Function " &
                  "Ada.Numerics.Float_Random.Value");
            end if;
            Float_Random.Reset (Fgen_1, Fstate_1);
         exception
            when Constraint_Error =>
               null;  -- OK, expected exception.
            when Program_Error =>
               null; -- OK, expected exception.
            when others =>
               Report.Failed
                 ("Unexpected exception raised by an " &
                  "instantiated version of " &
                  "Ada.Numerics.Float_Random.Value when " &
                  "provided a string input that does not " &
                  "represent the state of a random number " &
                  "generator");
         end;

      end Objective_3;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxa5012;
