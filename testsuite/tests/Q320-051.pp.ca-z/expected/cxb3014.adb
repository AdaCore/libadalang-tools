-- CXB3014.A
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
--      Check that the Function Value with Pointer and Element
--      parameters will return an Element_Array result of correct size
--      and content (up to and including the first "terminator" Element).
--
--      Check that the Function Value with Pointer and Length parameters
--      will return an Element_Array result of appropriate size and content
--      (the first Length elements pointed to by the parameter Ref).
--
--      Check that both versions of Function Value will propagate
--      Interfaces.C.Strings.Dereference_Error when the value of
--      the Ref pointer parameter is null.
--
-- TEST DESCRIPTION:
--      This test tests that both versions of Function Value from the
--      generic package Interfaces.C.Pointers are available and produce
--      correct results.  The generic package is instantiated with size_t,
--      char, char_array, and nul as actual parameters, and subtests are
--      performed on each of the Value functions resulting from this
--      instantiation.
--      For both function versions, a test is performed where a portion of
--      a char_array is to be returned as the function result.  Likewise,
--      a test is performed where each version of the function returns the
--      entire char_array referenced by the in parameter Ref.
--      Finally, both versions of Function Value are called with a null
--      pointer reference, to ensure that Dereference_Error is raised in
--      this case.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', 'a'..'z', and 'A'..'Z'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      packages Interfaces.C.Strings and Interfaces.C.Pointers.  If an
--      implementation provides packages Interfaces.C.Strings and
--      Interfaces.C.Pointers, this test must compile, execute, and
--      report "PASSED".
--
--
-- CHANGE HISTORY:
--      19 Oct 95   SAIC    Initial prerelease version.
--      13 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      23 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Interfaces.C.Strings;                                    -- N/A => ERROR
with Interfaces.C.Pointers;                                   -- N/A => ERROR

procedure Cxb3014 is

begin

   Report.Test
     ("CXB3014",
      "Check that versions of the Value function " &
      "from package Interfaces.C.Pointers produce " &
      "correct results");

   Test_Block : declare

      use type Interfaces.C.Char, Interfaces.C.Size_T;

      Char_A : constant Interfaces.C.Char := 'a';
      Char_J : constant Interfaces.C.Char := 'j';
      Char_Z : constant Interfaces.C.Char := 'z';

      subtype Lower_Case_Chars is Interfaces.C.Char range Char_A .. Char_Z;
      subtype Char_Range is Interfaces.C.Size_T range 0 .. 26;

      Local_Nul     : aliased Interfaces.C.Char := Interfaces.C.Nul;
      Tc_Array_Size : Interfaces.C.Size_T       := 20;

      Tc_String_1     : constant String := "abcdefghij";
      Tc_String_2     : constant String := "abcdefghijklmnopqrstuvwxyz";
      Tc_String_3     : constant String := "abcdefghijklmnopqrst";
      Tc_String_4     : constant String := "abcdefghijklmnopqrstuvwxyz";
      Tc_Blank_String : constant String := "                          ";

      Tc_Char_Array : Interfaces.C.Char_Array (Char_Range) :=
        Interfaces.C.To_C (Tc_String_2, True);

      Tc_Char_Array_1 : Interfaces.C.Char_Array (0 .. 9);
      Tc_Char_Array_2 : Interfaces.C.Char_Array (Char_Range);
      Tc_Char_Array_3 : Interfaces.C.Char_Array (0 .. Tc_Array_Size - 1);
      Tc_Char_Array_4 : Interfaces.C.Char_Array (Char_Range);

      package Char_Pointers is new Interfaces.C.Pointers
        (Index              => Interfaces.C.Size_T,
         Element            => Interfaces.C.Char,
         Element_Array      => Interfaces.C.Char_Array,
         Default_Terminator => Interfaces.C.Nul);

      Char_Ptr : Char_Pointers.Pointer;

      use type Char_Pointers.Pointer;

   begin

      -- Check that the Function Value with Pointer and Terminator Element
      -- parameters will return an Element_Array result of appropriate size
      -- and content (up to and including the first "terminator" Element.)

      Char_Ptr := Tc_Char_Array (0)'Access;

      -- Provide a new Terminator char in the call of Function Value.
      -- This call should return only a portion (the first 10 chars) of
      -- the referenced char_array, up to and including the char 'j'.

      Tc_Char_Array_1 :=
        Char_Pointers.Value (Ref => Char_Ptr, Terminator => Char_J);

      if Interfaces.C.To_Ada (Tc_Char_Array_1, False) /= Tc_String_1 or
        Interfaces.C.Is_Nul_Terminated (Tc_Char_Array_1)
      then
         Report.Failed
           ("Incorrect result from Function Value with Ref " &
            "and Terminator parameters, when supplied with " &
            "a non-default Terminator char");
      end if;

      -- Use the default Terminator char in the call of Function Value.
      -- This call should return the entire char_array, including the
      -- terminating nul char.

      Tc_Char_Array_2 := Char_Pointers.Value (Char_Ptr);

      if Interfaces.C.To_Ada (Tc_Char_Array_2, True) /= Tc_String_2 or
        not Interfaces.C.Is_Nul_Terminated (Tc_Char_Array_2)
      then
         Report.Failed
           ("Incorrect result from Function Value with Ref " &
            "and Terminator parameters, when using the " &
            "default Terminator char");
      end if;

      -- Check that the Function Value with Pointer and Length parameters
      -- will return an Element_Array result of appropriate size and content
      -- (the first Length elements pointed to by the parameter Ref).

      -- This call should return only a portion (the first 20 chars) of
      -- the referenced char_array.

      Tc_Char_Array_3 :=
        Char_Pointers.Value
          (Ref    => Char_Ptr,
           Length => Interfaces.C.Ptrdiff_T (Tc_Array_Size));

      -- Verify the individual chars of the result.
      for I in 0 .. Tc_Array_Size - 1 loop
         if Interfaces.C.To_Ada (Tc_Char_Array_3 (I)) /=
           Tc_String_3 (Integer (I) + 1)
         then
            Report.Failed
              ("Incorrect result from Function Value with " &
               "Ref and Length parameters, when specifying " &
               "a length less than the full array size");
            exit;
         end if;
      end loop;

      -- This call should return the entire char_array, including the
      -- terminating nul char.

      Tc_Char_Array_4 := Char_Pointers.Value (Char_Ptr, 27);

      if Interfaces.C.To_Ada (Tc_Char_Array_4, True) /= Tc_String_4 or
        not Interfaces.C.Is_Nul_Terminated (Tc_Char_Array_4)
      then
         Report.Failed
           ("Incorrect result from Function Value with Ref " &
            "and Length parameters, when specifying the " &
            "entire array size");
      end if;

      -- Check that both of the above versions of Function Value will
      -- propagate Interfaces.C.Strings.Dereference_Error when the value of
      -- the Ref Pointer parameter is null.

      Char_Ptr := null;

      begin
         Tc_Char_Array_1 :=
           Char_Pointers.Value (Ref => Char_Ptr, Terminator => Char_J);
         Report.Failed
           ("Dereference_Error not raised by Function " &
            "Value with Terminator parameter, when " &
            "provided a null reference");
         -- Call Report.Comment to ensure that the assignment to
         -- TC_Char_Array_1 is not "dead", and therefore can not be
         -- optimized away.
         Report.Comment (Interfaces.C.To_Ada (Tc_Char_Array_1, False));
      exception
         when Interfaces.C.Strings.Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Function " &
               "Value with Terminator parameter, when " &
               "provided a null reference");
      end;

      begin
         Tc_Char_Array_3 :=
           Char_Pointers.Value
             (Char_Ptr,
              Interfaces.C.Ptrdiff_T (Tc_Array_Size));
         Report.Failed
           ("Dereference_Error not raised by Function " &
            "Value with Length parameter, when provided " &
            "a null reference");
         -- Call Report.Comment to ensure that the assignment to
         -- TC_Char_Array_3 is not "dead", and therefore can not be
         -- optimized away.
         Report.Comment (Interfaces.C.To_Ada (Tc_Char_Array_3, False));
      exception
         when Interfaces.C.Strings.Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by Function " &
               "Value with Length parameter, when " &
               "provided a null reference");
      end;

   exception
      when others =>
         Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end Cxb3014;
