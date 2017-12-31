-- C37207A.ADA

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
-- OBJECTIVE:

--     FOR A TYPE WITH OR WITHOUT DEFAULT DISCRIMINANT VALUES, CHECK
--     THAT A DISCRIMINANT CONSTRAINT CAN BE SUPPLIED IN THE FOLLOWING
--     CONTEXTS AND HAS THE PROPER EFFECT:

--     IN A 1) OBJECT_DECLARATION, 2) COMPONENT_DECLARATION OR
--     3) SUBTYPE INDICATION OF AN ARRAY_TYPE_DEFINITION, AND HENCE,
--     ASSIGNMENTS CANNOT ATTEMPT TO CHANGE THE SPECIFIED DISCRIMINANT
--     VALUES WITHOUT RAISING CONSTRAINT_ERROR

--     4) IN AN ACCESS_TYPE_DEFINITION, AND HENCE, ACCESS VALUES
--     OF THIS ACCESS TYPE CANNOT BE ASSIGNED NON-NULL VALUES
--     DESIGNATING OBJECTS WITH DIFFERENT DISCRIMINANT VALUES.

--     5) IN AN ALLOCATOR, AND THE ALLOCATED OBJECT HAS THE SPECIFIED
--     DISCRIMINANT VALUES.

--     6) IN A FORMAL PARAMETER DECLARATION OF A SUBPROGRAM, AND
--     HENCE, ASSIGNMENTS TO THE FORMAL PARAMETER CANNOT ATTEMPT TO
--     CHANGE THE DISCRIMINANT VALUES WITHOUT RAISING CONSTRAINT_ERROR,
--     CONSTRAINED IS TRUE, AND IF ACTUAL PARAMETERS HAVE DISCRIMINANT
--     VALUES DIFFERENT FROM THE SPECIFIED ONES, CONSTRAINT_ERROR IS
--     RAISED.

-- HISTORY:

--     ASL 07/24/81
--     RJW 08/28/86  CORRECTED SYNTAX ERRORS.
--     JLH 08/07/87  ADDED CODE TO PREVENT DEAD VARIABLE OPTIMIZATION.
--     EDS 07/16/98  AVOID OPTIMIZATION

with Report; use Report;
procedure C37207a is

begin
   Test
     ("C37207A",
      "DISCRIMINANT CONSTRAINT CAN BE SUPPLIED TO " &
      "DECLARATIONS AND DEFINITIONS USING TYPES WITH OR WITHOUT " &
      "DEFAULT DISCRIMINANT VALUES");

   declare
      type Rec1 (Disc : Integer := 5) is record
         null;
      end record;

      type Rec2 (Disc : Integer) is record
         null;
      end record;

      Obj1    : Rec1 (6);                     -- 1.
      Obj2    : Rec2 (6);                     -- 1.
      Badobj1 : Rec1 (7);                  -- 1.
      Badobj2 : Rec2 (7);                  -- 1.

      type Rec3 is record
         Comp1 : Rec1 (6);          -- 2.
         Comp2 : Rec2 (6);          -- 2.
      end record;

      Obj3 : Rec3;

      type Arr1 is array (1 .. 10) of Rec1 (6);  -- 3.
      type Arr2 is array (1 .. 10) of Rec2 (6);  -- 3.

      A1 : Arr1;
      A2 : Arr2;

      type Rec1_Name is access Rec1 (6);   -- 4.
      type Rec2_Name is access Rec2 (6);   -- 4.

      Acc1 : Rec1_Name;
      Acc2 : Rec2_Name;

      subtype Rec16 is Rec1 (6);
      subtype Rec26 is Rec2 (6);

      procedure Proc
        (P1 : in out Rec16;     -- 6.
         P2 : in out Rec26) is  -- 6.
      begin
         if not (P1'Constrained and P2'Constrained) then  -- 6.
            Failed
              ("'CONSTRAINED ATTRIBUTE INCORRECT FOR " &
               "CONSTRAINED FORMAL PARAMETERS");
         end if;
         begin
            P1 := (Disc => 7);         -- 6.
            Failed
              ("CONSTRAINT_ERROR NOT RAISED UPON " &
               "ATTEMPT TO CHANGE DISCRIMINANT OF " &
               "CONSTRAINED FORMAL PARAMETER " & Integer'Image (P1.Disc));
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION     (1)");
         end;
         begin
            P2 := (Disc => 7);         -- 6.
            Failed
              ("CONSTRAINT_ERROR NOT RAISED UPON " &
               "ATTEMPT TO CHANGE DISCRIMINANT OF " &
               "CONSTRAINED FORMAL PARAMETER " & Integer'Image (P2.Disc));
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION     (2)");
         end;
      end Proc;
   begin
---------------------------------------------------------------

      begin
         Obj1 := (Disc => Ident_Int (7));           -- 1.
         Failed
           ("CONSTRAINT_ERROR NOT RAISED UPON " &
            "ATTEMPT TO CHANGE DISCRIMINANT OF " & "CONSTRAINED OBJECT");
         if Obj1 = (Disc => 7) then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION     (3)");
      end;

---------------------------------------------------------------

      begin
         Obj3 :=
           ((Disc => Ident_Int (7)),      -- 2.
            (Disc => Ident_Int (7)));     -- 2.
         Failed
           ("CONSTRAINT_ERROR NOT RAISED UPON " &
            "ATTEMPT TO CHANGE DISCRIMINANT OF " &
            "CONSTRAINED RECORD COMPONENT");
         if Obj3 = ((Disc => 7), (Disc => 7)) then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION     (4)");
      end;

--------------------------------------------------------------

      begin
         A2 (2) := (Disc => Ident_Int (7));          -- 3.
         Failed
           ("CONSTRAINT_ERROR NOT RAISED UPON " &
            "ATTEMPT TO CHANGE DISCRIMINANT OF " &
            "CONSTRAINED ARRAY COMPONENT");
         if A2 (2) = (Disc => 7) then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION     (5)");
      end;

--------------------------------------------------------------

      begin
         Acc1 := new Rec1 (Disc => Ident_Int (7));   -- 4.
         Failed
           ("CONSTRAINT_ERROR NOT RAISED UPON " &
            "ATTEMPT TO ASSIGN INCOMPATIBLE OBJECT " & "TO ACCESS VARIABLE");
         if Acc1 = new Rec1 (Disc => 7) then
            Comment ("PREVENTING DEAD VARIABLE OPTIMIZATION");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION     (6)");
      end;

----------------------------------------------------------------

      Acc1 := new Rec1 (Disc => Ident_Int (6));  -- OK.

      begin
         Acc1.all := Badobj1;           -- 5.
         Failed
           ("CONSTRAINT_ERROR NOT RAISED UPON " &
            "ATTEMPT TO ASSIGN INCOMPATIBLE OBJECT " & "TO ACCESSED OBJECT");
         if Acc1.all = Badobj1 then
            Comment ("PREVENT DEAD VARIABLE OPTIMIZATION");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION     (7)");
      end;

-----------------------------------------------------------------

      Proc (Obj1, Obj2);              -- OK.

      begin
         Proc (Badobj1, Badobj2);        -- 6.
         Failed
           ("CONSTRAINT_ERROR NOT RAISED UPON " &
            "PASSING OF CONSTRAINED ACTUAL " &
            "PARAMETERS TO DIFFERENTLY CONSTRAINED " & "FORMAL PARAMETERS");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION     (8)");
      end;

---------------------------------------------------------------
   end;

   Result;
end C37207a;
