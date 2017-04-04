-- CXB3016.A
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
--      Check that function Virtual_Length returns the number of elements
--      in the array referenced by the Pointer parameter Ref, up to (but
--      not including) the (first) instance of the element specified in
--      the Terminator parameter.
--
--      Check that the procedure Copy_Terminated_Array copies the array of
--      elements referenced by Pointer parameter Source, into the array
--      pointed to by parameter Target, based on which of the following
--      two scenarios occurs first:
--        1) copying the Terminator element, or
--        2) copying the number of elements specified in parameter Limit.
--
--      Check that procedure Copy_Terminated_Array will propagate
--      Dereference_Error if either the Source or Target parameter is null.
--
--      Check that procedure Copy_Array will copy an array of elements
--      of length specified in parameter Length, referenced by the
--      Pointer parameter Source, into the array pointed to by parameter
--      Target.
--
--      Check that procedure Copy_Array will propagate Dereference_Error
--      if either the Source or Target parameter is null.
--
-- TEST DESCRIPTION:
--      This test checks that the function Virtual_Length and the procedures
--      Copy_Terminated_Array and Copy_Array in the generic package
--      Interfaces.C.Pointers will allow the user to manipulate arrays of
--      char and short values through the pointers that reference the
--      arrays.
--
--      Package Interfaces.C.Pointers is instantiated twice, once for
--      short values and once for chars. Pointers from each instantiated
--      package are then used to reference arrays of the appropriate
--      element type.  The subprograms under test are used to determine the
--      length, and to copy, either portions or the entire content of the
--      arrays.  The results of these operations are then compared against
--      expected results.
--
--      The propagation of Dereference_Error is checked for when either
--      of the two procedures is supplied with a null Pointer parameter.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', and 'a'..'z'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      packages Interfaces.C, Interfaces.C.Strings, and
--      Interfaces.C.Pointers. If an implementation provides these packages,
--      this test must compile, execute, and report "PASSED".
--
--
-- CHANGE HISTORY:
--      01 Feb 96   SAIC    Initial release for 2.1
--      13 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Incorporated reviewer comments.
--      26 Feb 97   PWB.CTA Moved code using null pointer to avoid errors
--!

with Report;
with Ada.Exceptions;
with Interfaces.C;                                            -- N/A => ERROR
with Interfaces.C.Pointers;                                   -- N/A => ERROR
with Interfaces.C.Strings;                                    -- N/A => ERROR

procedure Cxb3016 is
begin

   Report.Test
     ("CXB3016",
      "Check that subprograms Virtual_Length, " &
      "Copy_Terminated_Array, and Copy_Array " &
      "produce correct results");

   Test_Block : declare

      use Ada.Exceptions;
      use Interfaces.C.Strings;

      use type
        Interfaces.C.Char,
        Interfaces.C.Char_Array,
        Interfaces.C.Ptrdiff_T,
        Interfaces.C.Short,
        Interfaces.C.Size_T;

      Tc_Char          : Interfaces.C.Char   := 'a';
      Tc_Ptrdiff_T     : Interfaces.C.Ptrdiff_T;
      Tc_Short         : Interfaces.C.Short  := 0;
      Min_Array_Size   : Interfaces.C.Size_T := 0;
      Max_Array_Size   : Interfaces.C.Size_T := 20;
      Short_Terminator : Interfaces.C.Short  := Interfaces.C.Short'Last;
      Alphabet         : constant String     := "abcdefghijklmnopqrstuvwxyz";
      Blank_String     : constant String     := "                          ";

      type Short_Array_Type is
        array (Interfaces.C.Size_T range <>) of aliased Interfaces.C.Short;

      Ch_Array : Interfaces.C
        .Char_Array
        (0 .. Interfaces.C.Size_T (Alphabet'Length)) :=
        Interfaces.C.To_C (Alphabet, True);

      Tc_Ch_Array : Interfaces.C
        .Char_Array
        (0 .. Interfaces.C.Size_T (Blank_String'Length)) :=
        Interfaces.C.To_C (Blank_String, True);

      Short_Array    : Short_Array_Type (Min_Array_Size .. Max_Array_Size);
      Tc_Short_Array : Short_Array_Type (Min_Array_Size .. Max_Array_Size);

      package Char_Pointers is new Interfaces.C.Pointers
        (Index              => Interfaces.C.Size_T,
         Element            => Interfaces.C.Char,
         Element_Array      => Interfaces.C.Char_Array,
         Default_Terminator => Interfaces.C.Nul);

      package Short_Pointers is new Interfaces.C.Pointers
        (Index              => Interfaces.C.Size_T,
         Element            => Interfaces.C.Short,
         Element_Array      => Short_Array_Type,
         Default_Terminator => Short_Terminator);

      use Short_Pointers, Char_Pointers;

      Short_Ptr    : Short_Pointers.Pointer := Short_Array (0)'Access;
      Tc_Short_Ptr : Short_Pointers.Pointer := Tc_Short_Array (0)'Access;
      Char_Ptr     : Char_Pointers.Pointer  := Ch_Array (0)'Access;
      Tc_Char_Ptr  : Char_Pointers.Pointer  := Tc_Ch_Array (0)'Access;

   begin

      -- Provide initial values for the array that holds short int values.

      for I in Min_Array_Size .. Max_Array_Size loop
         Short_Array (I)    := Interfaces.C.Short (I);
         Tc_Short_Array (I) := 100;
      end loop;

      -- Set the final element of the short array object to be the "terminator"
      -- element used in the instantiation above.

      Short_Array (Max_Array_Size) := Short_Terminator;

      -- Check starting pointer positions.

      if Short_Ptr.all /= 0 or Char_Ptr.all /= Ch_Array (0) then
         Report.Failed
           ("Incorrect initial value for the first " &
            "Char_Array or Short_Array values");
      end if;

      -- Check that function Virtual_Length returns the number of elements
      -- in the array referenced by the Pointer parameter Ref, up to (but
      -- not including) the (first) instance of the element specified in
      -- the Terminator parameter.

      Tc_Char := 'j';

      Tc_Ptrdiff_T :=
        Char_Pointers.Virtual_Length (Ref => Char_Ptr, Terminator => Tc_Char);
      if Tc_Ptrdiff_T /= 9 then
         Report.Failed
           ("Incorrect result from function Virtual_Length " &
            "with Char_ptr parameter - 1");
      end if;

      Tc_Char := Interfaces.C.Nul;

      Tc_Ptrdiff_T :=
        Char_Pointers.Virtual_Length (Char_Ptr, Terminator => Tc_Char);
      if Tc_Ptrdiff_T /= Interfaces.C.Ptrdiff_T (Alphabet'Length) then
         Report.Failed
           ("Incorrect result from function Virtual_Length " &
            "with Char_ptr parameter - 2");
      end if;

      Tc_Short := 10;

      Tc_Ptrdiff_T := Short_Pointers.Virtual_Length (Short_Ptr, Tc_Short);

      if Tc_Ptrdiff_T /= 10 then
         Report.Failed
           ("Incorrect result from function Virtual_Length " &
            "with Short_ptr parameter - 1");
      end if;

      -- Replace an element of the Short_Array with the element used as the
      -- terminator of the entire array; now there are two occurrences of the
      -- terminator element in the array.  The call to Virtual_Length should
      -- return the number of array elements prior to the first terminator.

      Short_Array (5) := Short_Terminator;

      if Short_Pointers.Virtual_Length (Short_Ptr, Short_Terminator) /= 5 then
         Report.Failed
           ("Incorrect result from function Virtual_Length " &
            "with Short_ptr parameter - 2");
      end if;

      -- Check that the procedure Copy_Terminated_Array copies the array of
      -- elements referenced by Pointer parameter Source, into the array
      -- pointed to by parameter Target, based on which of the following
      -- two scenarios occurs first:
      --   1) copying the Terminator element, or
      --   2) copying the number of elements specified in parameter Limit.
      -- Note: Terminator element must be copied to Target, as well as
      --       all array elements prior to the terminator element.

      if Tc_Ch_Array = Ch_Array then
         Report.Failed
           ("The two char arrays are equivalent prior to the " &
            "call to Copy_Terminated_Array - 1");
      end if;

      -- Case 1: Copying the Terminator Element.  (Default terminator)

      Char_Pointers.Copy_Terminated_Array
        (Source => Char_Ptr,
         Target => Tc_Char_Ptr);

      if Tc_Ch_Array /= Ch_Array then
         Report.Failed
           ("The two char arrays are not equal following the " &
            "call to Copy_Terminated_Array, case of copying " &
            "the Terminator Element, using default terminator");
      end if;

      -- Reset the Target Pointer array.

      Tc_Ch_Array := Interfaces.C.To_C (Blank_String, True);
      Tc_Char_Ptr := Tc_Ch_Array (0)'Access;

      if Tc_Ch_Array = Ch_Array then
         Report.Failed
           ("The two char arrays are equivalent prior to the " &
            "call to Copy_Terminated_Array - 2");
      end if;

      -- Case 2: Copying the Terminator Element.  (Non-Default terminator)

      Tc_Char := 'b';  -- Second char in char_array pointed to by Char_Ptr
      Char_Pointers.Copy_Terminated_Array
        (Source     => Char_Ptr,
         Target     => Tc_Char_Ptr,
         Terminator => Tc_Char);

      if Tc_Ch_Array (0) /= Ch_Array (0) or  -- Initial value modified.
        Tc_Ch_Array (1) /= Ch_Array (1) or  -- Initial value modified.
        Tc_Ch_Array (2) = Ch_Array (2) or  -- Initial value not modified.
        Tc_Ch_Array (5) = Ch_Array (5) or  -- Initial value not modified.
        Tc_Ch_Array (15) = Ch_Array (15) or  -- Initial value not modified.
        Tc_Ch_Array (25) = Ch_Array (25)     -- Initial value not modified.
      then
         Report.Failed
           ("The appropriate portions of the two char arrays " &
            "are not equal following the call to " &
            "Copy_Terminated_Array, case of copying the " &
            "Terminator Element, using non-default terminator");
      end if;

      if Tc_Short_Array = Short_Array then
         Report.Failed
           ("The two short int arrays are equivalent prior " &
            "to the call to Copy_Terminated_Array - 1");
      end if;

      Short_Pointers.Copy_Terminated_Array
        (Source     => Short_Ptr,
         Target     => Tc_Short_Ptr,
         Terminator => 2);

      if Tc_Short_Array (0) /= Short_Array (0) or
        Tc_Short_Array (1) /= Short_Array (1) or
        Tc_Short_Array (2) /= Short_Array (2) or
        Tc_Short_Array (3) /= 100  -- Initial value not modified.
      then
         Report.Failed
           ("The appropriate portions of the two short int " &
            "arrays are not equal following the call to " &
            "Copy_Terminated_Array, case of copying the " &
            "Terminator Element, using non-default terminator");
      end if;

      -- Case 3: Copying the number of elements specified in parameter Limit.

      if Tc_Short_Array = Short_Array then
         Report.Failed
           ("The two short int arrays are equivalent prior " &
            "to the call to Copy_Terminated_Array - 2");
      end if;

      Tc_Ptrdiff_T := 5;

      Short_Pointers.Copy_Terminated_Array
        (Source     => Short_Ptr,
         Target     => Tc_Short_Ptr,
         Limit      => Tc_Ptrdiff_T,
         Terminator => Short_Terminator);

      if Tc_Short_Array (0) /= Short_Array (0) or
        Tc_Short_Array (1) /= Short_Array (1) or
        Tc_Short_Array (2) /= Short_Array (2) or
        Tc_Short_Array (3) /= Short_Array (3) or
        Tc_Short_Array (4) /= Short_Array (4) or
        Tc_Short_Array (5) /= 100  -- Initial value not modified.
      then
         Report.Failed
           ("The appropriate portions of the two Short arrays " &
            "are not equal following the call to " &
            "Copy_Terminated_Array, case of copying the number " &
            "of elements specified in parameter Limit");
      end if;

      -- Case 4: Copying the number of elements specified in parameter Limit,
      --         which also happens to be the number of elements up to and
      --         including the first terminator.

      -- Reset initial values for the array that holds short int values.

      for I in Min_Array_Size .. Max_Array_Size loop
         Short_Array (I)    := Interfaces.C.Short (I);
         Tc_Short_Array (I) := 100;
      end loop;

      if Tc_Short_Array = Short_Array then
         Report.Failed
           ("The two short int arrays are equivalent prior " &
            "to the call to Copy_Terminated_Array - 3");
      end if;

      Tc_Ptrdiff_T     := 3;  -- Specifies three elements to be copied.
      Short_Terminator := 2;  -- Value held in Short_Array third element,
      -- will serve as the "terminator" element.

      Short_Pointers.Copy_Terminated_Array
        (Source     => Short_Ptr,
         Target     => Tc_Short_Ptr,
         Limit      => Tc_Ptrdiff_T,
         Terminator => Short_Terminator);

      if Tc_Short_Array (0) /= Short_Array (0) or  -- First element copied.
        Tc_Short_Array (1) /= Short_Array (1) or  -- Second element copied.
        Tc_Short_Array (2) /= Short_Array (2) or  -- Third element copied.
        Tc_Short_Array (3) /= 100  -- Initial value of fourth element
      then                         -- not modified.
         Report.Failed
           ("The appropriate portions of the two Short arrays " &
            "are not equal following the call to " &
            "Copy_Terminated_Array, case of copying the number " &
            "of elements specified in parameter " &
            "Limit, which also happens to be the number of " &
            "elements up to and including the first terminator");
      end if;

      -- Check that procedure Copy_Terminated_Array will propagate
      -- Dereference_Error if either the Source or Target parameter is null.

      Char_Ptr := null;
      begin
         Char_Pointers.Copy_Terminated_Array (Char_Ptr, Tc_Char_Ptr);
         Report.Failed
           ("Dereference_Error not raised by call to " &
            "Copy_Terminated_Array with null Source parameter");
         if Tc_Char_Ptr = null then  -- To avoid optimization.
            Report.Comment ("This should never be printed");
         end if;
      exception
         when Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by call to " &
               "Copy_Terminated_Array with null Source parameter");
      end;

      Tc_Short_Ptr := null;
      begin
         Short_Pointers.Copy_Terminated_Array (Short_Ptr, Tc_Short_Ptr);
         Report.Failed
           ("Dereference_Error not raised by call to " &
            "Copy_Terminated_Array with null Target parameter");
         if Short_Ptr = null then  -- To avoid optimization.
            Report.Comment ("This should never be printed");
         end if;
      exception
         when Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by call to " &
               "Copy_Terminated_Array with null Target parameter");
      end;

      -- Check that the procedure Copy_Array will copy the array of
      -- elements of length specified in parameter Length, referenced by
      -- the Pointer parameter Source, into the array pointed to by
      -- parameter Target.

      -- Reinitialize Target arrays prior to test cases below.

      Tc_Ch_Array := Interfaces.C.To_C (Blank_String, True);

      for I in Min_Array_Size .. Max_Array_Size loop
         Tc_Short_Array (I) := 100;
      end loop;

      Char_Ptr     := Ch_Array (0)'Access;
      Tc_Char_Ptr  := Tc_Ch_Array (0)'Access;
      Short_Ptr    := Short_Array (0)'Access;
      Tc_Short_Ptr := Tc_Short_Array (0)'Access;

      Tc_Ptrdiff_T := 4;

      Char_Pointers.Copy_Array
        (Source => Char_Ptr,
         Target => Tc_Char_Ptr,
         Length => Tc_Ptrdiff_T);

      if Tc_Ch_Array (0) /= Ch_Array (0) or
        Tc_Ch_Array (1) /= Ch_Array (1) or
        Tc_Ch_Array (2) /= Ch_Array (2) or
        Tc_Ch_Array (3) /= Ch_Array (3) or
        Tc_Ch_Array (4) = Ch_Array (4)
      then
         Report.Failed
           ("Incorrect result from Copy_Array when using " &
            "char pointer arguments, partial array copied");
      end if;

      Tc_Ptrdiff_T := Interfaces.C.Ptrdiff_T (Max_Array_Size) + 1;

      Short_Pointers.Copy_Array (Short_Ptr, Tc_Short_Ptr, Tc_Ptrdiff_T);

      if Tc_Short_Array /= Short_Array then
         Report.Failed
           ("Incorrect result from Copy_Array when using Short " &
            "pointer arguments, entire array copied");
      end if;

      -- Check that procedure Copy_Array will propagate Dereference_Error
      -- if either the Source or Target parameter is null.

      Char_Ptr := null;
      begin
         Char_Pointers.Copy_Array (Char_Ptr, Tc_Char_Ptr, Tc_Ptrdiff_T);
         Report.Failed
           ("Dereference_Error not raised by call to " &
            "Copy_Array with null Source parameter");
         if Tc_Char_Ptr = null then  -- To avoid optimization.
            Report.Comment ("This should never be printed");
         end if;
      exception
         when Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by call to " &
               "Copy_Array with null Source parameter");
      end;

      Tc_Short_Ptr := null;
      begin
         Short_Pointers.Copy_Array (Short_Ptr, Tc_Short_Ptr, Tc_Ptrdiff_T);
         Report.Failed
           ("Dereference_Error not raised by call to " &
            "Copy_Array with null Target parameter");
         if Short_Ptr = null then  -- To avoid optimization.
            Report.Comment ("This should never be printed");
         end if;
      exception
         when Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by call to " &
               "Copy_Array with null Target parameter");
      end;

      -- Check that function Virtual_Length will propagate Dereference_Error
      -- if the Source parameter is null.

      Char_Ptr := null;
      begin
         Tc_Ptrdiff_T :=
           Char_Pointers.Virtual_Length (Char_Ptr, Terminator => Tc_Char);
         Report.Failed
           ("Dereference_Error not raised by call to " &
            "Virtual_Length with null Source parameter");
         if Tc_Ptrdiff_T = 100 then    -- To avoid optimization.
            Report.Comment ("This should never be printed");
         end if;
      exception
         when Dereference_Error =>
            null;  -- OK, expected exception.
         when others =>
            Report.Failed
              ("Incorrect exception raised by call to " &
               "Virtual_Length with null Source parameter");
      end;

   exception
      when The_Error : others =>
         Report.Failed
           ("The following exception was raised in the " &
            "Test_Block: " &
            Exception_Name (The_Error));
   end Test_Block;

   Report.Result;

end Cxb3016;
