
--  Based on ARM 2022
--  $3.1:
--  basic_declaration ::= 
--       type_declaration | subtype_declaration
--     | object_declaration | number_declaration
--     | subprogram_declaration | abstract_subprogram_declaration
--     | null_procedure_declaration | expression_function_declaration
--     | package_declaration | renaming_declaration
--     | exception_declaration | generic_declaration
--     | generic_instantiation
--  $3.2.1:
--  type_declaration ::=  full_type_declaration
--     | incomplete_type_declaration
--     | private_type_declaration
--     | private_extension_declaration
--  $3.2.1:
--  full_type_declaration ::= 
--       type defining_identifier [known_discriminant_part] is type_definition
--          [aspect_specification];
--     | task_type_declaration
--     | protected_type_declaration

--  This testcase concerns simple task type declaration.

with Ada.Text_IO; use Ada.Text_IO;

procedure Simple_Task_Type_Decl is
   task type TT;

   task body TT is
   begin
      Put_Line ("In task TT");
   end TT;
     
   A_Task : TT;

begin
   Put_Line ("In main");
end Simple_Task_Type_Decl;
