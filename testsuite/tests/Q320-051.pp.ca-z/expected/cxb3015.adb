-- CXB3015.A
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
--      Check that the "+" and "-" functions with Pointer and ptrdiff_t
--      parameters that return Pointer values produce correct results,
--      based on the size of the array elements.
--
--      Check that the "-" function with two Pointer parameters that
--      returns a ptrdiff_t type parameter produces correct results,
--      based on the size of the array elements.
--
--      Check that each of the "+" and "-" functions above will
--      propagate Pointer_Error if a Pointer parameter is null.
--
--      Check that the Increment and Decrement procedures provide the
--      correct "pointer arithmetic" operations.
--
-- TEST DESCRIPTION:
--      This test checks that the functions "+" and "-", and the procedures
--      Increment and Decrement in the generic package Interfaces.C.Pointers
--      will allow the user to perform "pointer arithmetic" operations on
--      Pointer values.
--      Package Interfaces.C.Pointers is instantiated three times, for
--      short values, chars, and arrays of arrays.  Pointers from each
--      instantiated package are then used to reference different elements
--      of array objects.  Pointer arithmetic operations are performed on
--      these pointers, and the results of these operations are verified
--      against expected pointer positions along the referenced arrays.
--      The propagation of Pointer_Error is checked for when the function
--      Pointer parameter is null.
--
--      The following chart indicates the combinations of subprograms and
--      parameter types used in this test.
--
--
--                                 Short    Char    Array
--                               --------------------------
--      "+" Pointer, ptrdiff_t  |   X    |        |   X    |
--                              |--------------------------|
--      "+" ptrdiff_t, Pointer  |   X    |        |   X    |
--                              |--------------------------|
--      "-" Pointer, ptrdiff_t  |        |   X    |   X    |
--                              |--------------------------|
--      "-" Pointer, Pointer    |        |   X    |   X    |
--                              |--------------------------|
--      Increment (Pointer)     |   X    |        |   X    |
--                              |--------------------------|
--      Decrement (Pointer)     |   X    |        |   X    |
--                               --------------------------
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', and 'a'..'z'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      package Interfaces.C.Pointers.  If an implementation provides
--      package Interfaces.C.Pointers, this test must compile, execute, and
--      report "PASSED".
--
--
-- CHANGE HISTORY:
--      26 Oct 95   SAIC    Initial prerelease version.
--      13 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Incorporated reviewer comments.
--      06 Mar 00   RLB     Repaired so that array of arrays component
--                          type is statically constrained. (C does not have
--                          an analog to an array of dynamically constrained
--                          arrays.)

with Report;
with Ada.Exceptions;
with Interfaces.C.Pointers;                                   -- N/A => ERROR

procedure Cxb3015 is
begin

   Report.Test
     ("CXB3015",
      "Check that +, -, Increment, and Decrement " &
      "subprograms in Package Interfaces.C.Pointers " &
      "produce correct results");

   Test_Block :
   declare

      use Ada.Exceptions;
      use type Interfaces.C.Short;
      use type Interfaces.C.Size_T, Interfaces.C.Ptrdiff_T;
      use type Interfaces.C.Char, Interfaces.C.Char_Array;

      Tc_Count                : Interfaces.C.Size_T;
      Tc_Increment            : Interfaces.C.Ptrdiff_T;
      Tc_Ptrdiff_T            : Interfaces.C.Ptrdiff_T;
      Tc_Short                : Interfaces.C.Short           := 0;
      Tc_Verbose              : Boolean                      := False;
      Constant_Min_Array_Size : constant Interfaces.C.Size_T := 0;
      Constant_Max_Array_Size : constant Interfaces.C.Size_T := 20;
      Min_Array_Size          : Interfaces.C.Size_T          :=
        Interfaces.C.Size_T
          (Report.Ident_Int (Integer (Constant_Min_Array_Size)));
      Max_Array_Size : Interfaces.C.Size_T :=
        Interfaces.C.Size_T
          (Report.Ident_Int (Integer (Constant_Max_Array_Size)));
      Min_Size_T, Max_Size_T : Interfaces.C.Size_T;
      Short_Terminator       : Interfaces.C.Short := Interfaces.C.Short'Last;
      Alphabet               : constant String := "abcdefghijklmnopqrstuvwxyz";

      type Short_Array_Type is
        array (Interfaces.C.Size_T range <>) of aliased Interfaces.C.Short;

      type Constrained_Array_Type is
        array (Min_Array_Size .. Max_Array_Size) of aliased Interfaces.C.Short;

      type Static_Constrained_Array_Type is
        array
          (Constant_Min_Array_Size ..
               Constant_Max_Array_Size) of aliased Interfaces.C.Short;

      type Array_Of_Arrays_Type is
        array
          (Interfaces.C
             .Size_T range <>) of aliased Static_Constrained_Array_Type;

      Short_Array : Short_Array_Type (Min_Array_Size .. Max_Array_Size);

      Constrained_Array : Constrained_Array_Type;

      Terminator_Array : Static_Constrained_Array_Type :=
        (others => Short_Terminator);

      Ch_Array : Interfaces.C.Char_Array
        (0 .. Interfaces.C.Size_T (Alphabet'Length)) :=
        Interfaces.C.To_C (Alphabet, True);

      Array_Of_Arrays : Array_Of_Arrays_Type
        (Min_Array_Size .. Max_Array_Size);

      package Short_Pointers is new Interfaces.C.Pointers
        (Index => Interfaces.C.Size_T, Element => Interfaces.C.Short,
         Element_Array      => Short_Array_Type,
         Default_Terminator => Short_Terminator);

      package Char_Pointers is new Interfaces.C.Pointers (Interfaces.C.Size_T,
         Interfaces.C.Char, Element_Array => Interfaces.C.Char_Array,
         Default_Terminator               => Interfaces.C.Nul);

      package Array_Pointers is new Interfaces.C.Pointers (Interfaces.C.Size_T,
         Static_Constrained_Array_Type, Array_Of_Arrays_Type,
         Terminator_Array);

      use Short_Pointers, Char_Pointers, Array_Pointers;

      Short_Ptr       : Short_Pointers.Pointer := Short_Array (0)'Access;
      Char_Ptr        : Char_Pointers.Pointer  := Ch_Array (0)'Access;
      Start_Char_Ptr  : Char_Pointers.Pointer  := Ch_Array (1)'Access;
      End_Char_Ptr    : Char_Pointers.Pointer  := Ch_Array (10)'Access;
      Array_Ptr       : Array_Pointers.Pointer := Array_Of_Arrays (0)'Access;
      Start_Array_Ptr : Array_Pointers.Pointer := Array_Of_Arrays (1)'Access;
      End_Array_Ptr   : Array_Pointers.Pointer := Array_Of_Arrays (10)'Access;

   begin

      -- Provide initial values for the arrays that hold short int values.

      for I in Min_Array_Size .. Max_Array_Size - 1 loop
         Short_Array (I) := Interfaces.C.Short (I);
         for J in Min_Array_Size .. Max_Array_Size loop
            -- Initialize this "array of arrays" so that element (i)(0) is
            -- different for each value of i.
            Array_Of_Arrays (I) (J) := Tc_Short;
            Tc_Short                := Tc_Short + 1;
         end loop;
      end loop;

      -- Set the final element of each array object to be the "terminator"
      -- element used in the instantiations above.

      Short_Array (Max_Array_Size)     := Short_Terminator;
      Array_Of_Arrays (Max_Array_Size) := Terminator_Array;

      -- Check starting pointer positions.

      if Short_Ptr.all /= 0 or Char_Ptr.all /= Ch_Array (0) or
        Array_Ptr.all /= Array_Of_Arrays (0) then
         Report.Failed
           ("Incorrect initial value for the first " &
            "Short_Array, Ch_Array, or Array_of_Array values");
      end if;

      -- Check that both versions of the "+" function with Pointer and
      -- ptrdiff_t parameters, that return a Pointer value, produce
      -- correct results, based on the size of the array elements.

      for I in Min_Array_Size + 1 .. Max_Array_Size loop

         if Integer (I) / 2 * 2 /= Integer (I) then -- Odd numbered loops.
            -- Pointer + ptrdiff_t, increment by 1.
            Short_Ptr := Short_Ptr + 1;
         else                                  -- Even numbered loops.
            -- ptrdiff_t + Pointer, increment by 1.
            Short_Ptr := 1 + Short_Ptr;
         end if;

         if Short_Ptr.all /= Short_Array (I) then
            Report.Failed
              ("Incorrect value returned following use " &
               "of the function +, incrementing by 1, " & "array position : " &
               Integer'Image (Integer (I)));
            if not Tc_Verbose then
               exit;
            end if;
         end if;
      end loop;

      Array_Ptr    := Array_Of_Arrays (Min_Array_Size)'Access;
      Tc_Count     := Min_Array_Size;
      Tc_Increment := 3;
      while Tc_Count + Interfaces.C.Size_T (Tc_Increment) < Max_Array_Size loop

         if Integer (Tc_Count) / 2 * 2 /= Integer (Tc_Count) then
            -- Odd numbered loops.
            -- Pointer + ptrdiff_t, increment by 3.
            Array_Ptr := Array_Pointers."+" (Array_Ptr, Tc_Increment);
         else
            -- Odd numbered loops.
            -- ptrdiff_t + Pointer, increment by 3.
            Array_Ptr :=
              Array_Pointers."+" (Left => Tc_Increment, Right => Array_Ptr);
         end if;

         if Array_Ptr.all /=
           Array_Of_Arrays (Tc_Count + Interfaces.C.Size_T (Tc_Increment)) then
            Report.Failed
              ("Incorrect value returned following use " &
               "of the function +, incrementing by " &
               Integer'Image (Integer (Tc_Increment)) & ", array position : " &
               Integer'Image (Integer (Tc_Count) + Integer (Tc_Increment)));
            if not Tc_Verbose then
               exit;
            end if;
         end if;

         Tc_Count := Tc_Count + Interfaces.C.Size_T (Tc_Increment);
      end loop;

      -- Check that the "-" function with Pointer and ptrdiff_t parameters,
      -- that returns a Pointer result, produces correct results, based on
      -- the size of the array elements.

      -- Set the pointer to the last element in the char_array, which is a nul
      -- char.
      Char_Ptr := Ch_Array (Interfaces.C.Size_T (Alphabet'Length))'Access;

      if Char_Ptr.all /= Interfaces.C.Nul then
         Report.Failed
           ("Incorrect initial value for the last " & "Ch_Array value");
      end if;

      Min_Size_T := 1;
      Max_Size_T := Interfaces.C.Size_T (Alphabet'Length);

      for I in reverse Min_Size_T .. Max_Size_T loop

         -- Subtract 1 from the pointer; it should now point to the previous
         -- element in the array.
         Char_Ptr := Char_Ptr - 1;

         if Char_Ptr.all /= Ch_Array (I - 1) then
            Report.Failed
              ("Incorrect value returned following use " &
               "of the function '-' with char element values, " &
               "array position : " & Integer'Image (Integer (I - 1)));
            if not Tc_Verbose then
               exit;
            end if;
         end if;
      end loop;

      Array_Ptr    := Array_Of_Arrays (Max_Array_Size)'Access;
      Tc_Count     := Max_Array_Size;
      Tc_Increment := 3;
      while Tc_Count > Min_Array_Size + Interfaces.C.Size_T (Tc_Increment) loop

         -- Decrement the pointer by 3.
         Array_Ptr := Array_Pointers."-" (Array_Ptr, Right => 3);

         if Array_Ptr.all /=
           Array_Of_Arrays (Tc_Count - Interfaces.C.Size_T (Tc_Increment)) then
            Report.Failed
              ("Incorrect value returned following use " &
               "of the function -, decrementing by " &
               Integer'Image (Integer (Tc_Increment)) & ", array position : " &
               Integer'Image (Integer (Tc_Count - 3)));
            if not Tc_Verbose then
               exit;
            end if;
         end if;

         Tc_Count := Tc_Count - Interfaces.C.Size_T (Tc_Increment);
      end loop;

      -- Check that the "-" function with two Pointer parameters, that returns
      -- a ptrdiff_t type result, produces correct results, based on the size
      -- of the array elements.

      Tc_Ptrdiff_T := 9;
      if Char_Pointers."-" (Left => End_Char_Ptr, Right => Start_Char_Ptr) /=
        Tc_Ptrdiff_T then
         Report.Failed
           ("Incorrect result from pointer-pointer " & "subtraction - 1");
      end if;

      Start_Char_Ptr := Ch_Array (1)'Access;
      End_Char_Ptr   := Ch_Array (25)'Access;

      Tc_Ptrdiff_T := 24;
      if Char_Pointers."-" (End_Char_Ptr, Right => Start_Char_Ptr) /=
        Tc_Ptrdiff_T then
         Report.Failed
           ("Incorrect result from pointer-pointer " & "subtraction - 2");
      end if;

      Tc_Ptrdiff_T := 9;
      if Array_Pointers."-" (End_Array_Ptr, Start_Array_Ptr) /= Tc_Ptrdiff_T
      then
         Report.Failed
           ("Incorrect result from pointer-pointer " & "subtraction - 3");
      end if;

      Start_Array_Ptr := Array_Of_Arrays (Min_Array_Size)'Access;
      End_Array_Ptr   := Array_Of_Arrays (Max_Array_Size)'Access;

      Tc_Ptrdiff_T :=
        Interfaces.C.Ptrdiff_T (Max_Array_Size) -
        Interfaces.C.Ptrdiff_T (Min_Array_Size);
      if End_Array_Ptr - Start_Array_Ptr /= Tc_Ptrdiff_T then
         Report.Failed
           ("Incorrect result from pointer-pointer " & "subtraction - 4");
      end if;

      -- Check that the Increment procedure produces correct results, based
      -- upon the size of the array elements.

      Short_Ptr := Short_Array (0)'Access;

      for I in Min_Array_Size + 1 .. Max_Array_Size loop
         -- Increment the value of the Pointer; it should now point to the next
         -- element in the array.
         Increment (Ref => Short_Ptr);

         if Short_Ptr.all /= Short_Array (I) then
            Report.Failed
              ("Incorrect value returned following use " &
               "of the Procedure Increment on pointer to an " &
               "array of short values, array position : " &
               Integer'Image (Integer (I)));
            if not Tc_Verbose then
               exit;
            end if;
         end if;
      end loop;

      Array_Ptr := Array_Of_Arrays (0)'Access;

      for I in Min_Array_Size + 1 .. Max_Array_Size loop
         -- Increment the value of the Pointer; it should now point to the next
         -- element in the array.
         Increment (Array_Ptr);

         if Array_Ptr.all /= Array_Of_Arrays (I) then
            Report.Failed
              ("Incorrect value returned following use " &
               "of the Procedure Increment on an array of " &
               "arrays, array position : " & Integer'Image (Integer (I)));
            if not Tc_Verbose then
               exit;
            end if;
         end if;
      end loop;

      -- Check that the Decrement procedure produces correct results, based
      -- upon the size of the array elements.

      Short_Ptr := Short_Array (Max_Array_Size)'Access;

      for I in reverse Min_Array_Size .. Max_Array_Size - 1 loop
         -- Decrement the value of the Pointer; it should now point to the
         -- previous element in the array.
         Decrement (Ref => Short_Ptr);

         if Short_Ptr.all /= Short_Array (I) then
            Report.Failed
              ("Incorrect value returned following use " &
               "of the Procedure Decrement on pointer to an " &
               "array of short values, array position : " &
               Integer'Image (Integer (I)));
            if not Tc_Verbose then
               exit;
            end if;
         end if;
      end loop;

      Array_Ptr := Array_Of_Arrays (Max_Array_Size)'Access;

      for I in reverse Min_Array_Size .. Max_Array_Size - 1 loop
         -- Decrement the value of the Pointer; it should now point to the
         -- previous array element.
         Decrement (Array_Ptr);

         if Array_Ptr.all /= Array_Of_Arrays (I) then
            Report.Failed
              ("Incorrect value returned following use " &
               "of the Procedure Decrement on an array of " &
               "arrays, array position : " & Integer'Image (Integer (I)));
            if not Tc_Verbose then
               exit;
            end if;
         end if;
      end loop;

      -- Check that each of the "+" and "-" functions above will propagate
      -- Pointer_Error if a Pointer parameter is null.

      begin
         Short_Ptr := null;
         Short_Ptr := Short_Ptr + 4;
         Report.Failed
           ("Pointer_Error not raised by Function + when " &
            "the Pointer parameter is null");
         if Short_Ptr /= null then  -- To avoid optimization.
            Report.Comment ("This should never be printed");
         end if;
      exception
         when Short_Pointers.Pointer_Error =>
            null; -- OK, expected exception.
         when others =>
            Report.Failed
              ("Unexpected exception raised by Function + " &
               "when the Pointer parameter is null");
      end;

      begin
         Char_Ptr := null;
         Char_Ptr := Char_Ptr - 1;
         Report.Failed
           ("Pointer_Error not raised by Function - when " &
            "the Pointer parameter is null");
         if Char_Ptr /= null then  -- To avoid optimization.
            Report.Comment ("This should never be printed");
         end if;
      exception
         when Char_Pointers.Pointer_Error =>
            null; -- OK, expected exception.
         when others =>
            Report.Failed
              ("Unexpected exception raised by Function - " &
               "when the Pointer parameter is null");
      end;

      begin
         Array_Ptr := null;
         Decrement (Array_Ptr);
         Report.Failed
           ("Pointer_Error not raised by Procedure Decrement " &
            "when the Pointer parameter is null");
         if Array_Ptr /= null then  -- To avoid optimization.
            Report.Comment ("This should never be printed");
         end if;
      exception
         when Array_Pointers.Pointer_Error =>
            null; -- OK, expected exception.
         when others =>
            Report.Failed
              ("Unexpected exception raised by Procedure " &
               "Decrement when the Pointer parameter is null");
      end;

   exception
      when The_Error : others =>
         Report.Failed
           ("The following exception was raised in the " & "Test_Block: " &
            Exception_Name (The_Error));
   end Test_Block;

   Report.Result;

end Cxb3015;
