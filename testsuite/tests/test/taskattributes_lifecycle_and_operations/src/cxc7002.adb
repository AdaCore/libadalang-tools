--  Removed unrelated with clauses...
with Ada.Task_Identification;
with Ada.Task_Attributes;
with CXC7002_0;  use type CXC7002_0.Int_Array;
with CXC7002_1;

procedure CXC7002 is
   package TID renames Ada.Task_Identification;
   generic package TA renames Ada.Task_Attributes;
   package ATA renames CXC7002_1;
   package ITA is new TA (Integer, 321);
begin
   --  Removed testsuite code as it is not relevant for gnattest...
   null;
end CXC7002;
