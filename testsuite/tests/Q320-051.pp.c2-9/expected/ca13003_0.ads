-- CA13003.A
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
--      Check that separate subunits which share an ancestor may have the
--      same name if they have different fully qualified names.  Check
--      the case of separate subunits of separate subunits.
--      This test is a change in semantics from Ada 83 to Ada 9X.
--
-- TEST DESCRIPTION:
--      Declare a package that provides file processing operations.  Declare
--      one separate package to do the file processing, and another to do the
--      auditing.  These packages contain similar functions declared in
--      separate subunits.  Verify that the main program can call the
--      separate subunits with the same name.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

-- Simulates a file processing application. The processing package opens files,
-- reads files, does file processing, and generates reports. The auditing
-- package opens files, read files, and generates reports.

package Ca13003_0 is

   type File_Id is range 1 .. 100;
   subtype File_Name is String (1 .. 10);

   Tc_Open_For_Process    : Boolean := False;
   Tc_Open_For_Audit      : Boolean := False;
   Tc_Report_From_Process : Boolean := False;
   Tc_Report_From_Audit   : Boolean := False;

   type File_Rec is record
      Name : File_Name;
      Id   : File_Id;
   end record;

   procedure Initialize_File_Rec
     (Name_In : in     File_Name;
      Id_In   : in     File_Id;
      File_In :    out File_Rec);

   ----------------------------------------------------------------------

   package Ca13003_1 is    -- File processing

      procedure Ca13003_3;                             -- Open files
      function Ca13003_4
        (Id_In   : File_Id;
         File_In : File_Rec)
        return File_Name;                              -- Process files
      package Ca13003_5 is                             -- Generate report
         procedure Generate_Report;
      end Ca13003_5;

   end Ca13003_1;

   ----------------------------------------------------------------------

   package Ca13003_2 is    -- File auditing

      procedure Ca13003_3;                             -- Open files
      function Ca13003_4
        (Id_In   : File_Id;
         File_In : File_Rec)
        return File_Name;                              -- Process files
      package Ca13003_5 is                             -- Generate report
         procedure Generate_Report;
      end Ca13003_5;

   end Ca13003_2;

end Ca13003_0;
