-- FC50A00.A
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
-- FOUNDATION DESCRIPTION:
--      This foundation declares various tagged types which will be passed as
--      actuals to generic formal tagged private types. It also declares
--      various objects of these types, which will be used for testing.
--      The types defined are both discriminated and nondiscriminated.
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

package Fc50a00 is

--
-- Nonlimited tagged types:
--

   type Count_Type is tagged record                     -- Nondiscriminated
      Count : Integer := 0;                             -- type.
   end record;

   subtype Str_Len is Natural range 0 .. 100;
   subtype Stu_Id is String (1 .. 5);
   subtype Dept_Id is String (1 .. 4);
   subtype Emp_Id is String (1 .. 9);
   type Status is (Student, Faculty, Staff);
   subtype Reserved is Positive range 1 .. 50;

   type Person_Type
     (Stat             : Status;                     -- Discriminated
      Namelen, Addrlen : Str_Len)
   is     -- type.
   tagged record
      Name    : String (1 .. Namelen);
      Address : String (1 .. Addrlen);
      case Stat is
         when Student =>
            Student_Id : Stu_Id;
         when Faculty =>
            Department : Dept_Id;
         when Staff =>
            Employee_Id : Emp_Id;
      end case;
   end record;

   type Viperson_Type is new Person_Type with record    -- Extension of
      Parking_Space : Reserved;                         -- discriminated type.
   end record;

   -- Testing entities: ------------------------------------------------

   Tc_Count_Item    : constant Count_Type := (Count => 111);
   Tc_Default_Count : constant Count_Type := (Count => 0);

   Tc_Person_Item : constant Person_Type :=
     (Faculty, 18, 17, "Eccles, John Scott", "Popham House, Lee", "0931");
   Tc_Default_Person : constant Person_Type :=
     (Student, 0, 0, "", "", "00000");

   Tc_Viperson_Item : constant Viperson_Type := (Tc_Person_Item with 1);

   ---------------------------------------------------------------------

end Fc50a00;
