--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Matrix.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Matrix.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Matrix (Gnattest_T : in out Test);
   procedure Test_Add_Matrix_7e2458 (Gnattest_T : in out Test) renames Test_Add_Matrix;
--  id:2.2/7e2458b3356557fd/Add_Matrix/1/0/
   procedure Test_Add_Matrix (Gnattest_T : in out Test) is
   --  matrix.ads:12:4:Add_Matrix
--  end read only

      pragma Unreferenced (Gnattest_T);

      Matrix_1 : Matrix_Type :=
        ((1, 0, 0, -1),
         (0, 1, 0, 0),
         (0, 0, 1, 0),
         (-1, 0, 0, 1));

      Matrix_2 : Matrix_Type :=
        ((0, 0, 0, 0),
         (0, 0, 0, 0),
         (0, 0, 0, 0),
         (0, 0, 0, 0));
   begin

      AUnit.Assertions.Assert
        (Add_Matrix (Matrix_1, Matrix_2) = Matrix_1,
         "adding zero matrix changes things");

--  begin read only
   end Test_Add_Matrix;
--  end read only


--  begin read only
   procedure Test_Multiply_Matrix (Gnattest_T : in out Test);
   procedure Test_Multiply_Matrix_9ea73c (Gnattest_T : in out Test) renames Test_Multiply_Matrix;
--  id:2.2/9ea73cc71730f240/Multiply_Matrix/1/0/
   procedure Test_Multiply_Matrix (Gnattest_T : in out Test) is
   --  matrix.ads:14:4:Multiply_Matrix
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Multiply_Matrix;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Matrix.Test_Data.Tests;
