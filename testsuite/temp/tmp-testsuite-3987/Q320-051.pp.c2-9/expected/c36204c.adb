-- C36204C.ADA

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
--     CHECK THAT THE 'RANGE ATTRIBUTE CAN BE USED TO DECLARE OBJECTS
--     AND IN A SUBTYPE AND TYPE DECLARATION.

-- HISTORY:
--     LB  08/13/86  CREATED ORIGINAL TEST.
--     BCB 08/18/87  CHANGED HEADER TO STANDARD HEADER FORMAT.
--                   REARRANGED STATEMENTS SO TEST IS CALLED FIRST.
--                   ELIMINATED DEAD VARIABLE OPTIMIZATION.  CHECKED
--                   RANGE VALUES FOR A SMALL INTEGER.

with Report; use Report;
procedure C36204c is

begin
   Test
     ("C36204C",
      "USING 'RANGE TO DECLARE OBJECTS AND " &
      "IN A SUBTYPE AND TYPE DECLARATION " &
      "RETURNS THE CORRECT VALUES.");

   declare

      Arr  : array (Ident_Int (4) .. Ident_Int (10)) of Integer;
      Obj1 : array (Arr'Range) of Boolean;

      subtype Small_Int is Integer range Arr'Range;
      Sml : Small_Int;

      type Other_Arr is array (Arr'Range) of Character;
      Obj2 : Other_Arr;

      type Arr_Type is
        array (Integer range Ident_Int (1) .. Ident_Int (10)) of Integer;
      type Arr_Ptr is access Arr_Type;
      Ptr : Arr_Ptr := new Arr_Type'(Arr_Type'Range => 0);

      function F return Arr_Type is
         Ar : Arr_Type := (Arr_Type'Range => 0);
      begin
         return Ar;
      end F;

   begin
      begin
         if Obj1'First /= Ident_Int (4) then
            Failed ("INCORRECT RANGE VALUE FOR AN OBJECT " & "DECLARATION 1");
         end if;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED WHEN CHECKING " & "OBJECT DECLARATION 1");
      end;

      begin
         if Obj1'Last /= Ident_Int (10) then
            Failed ("INCORRECT RANGE VALUE FOR AN OBJECT " & "DECLARATION 2");
         end if;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED WHEN CHECKING " & "OBJECT DECLARATION 2");
      end;

      begin
         if Small_Int'First /= 4 then
            Failed
              ("INCORRECT RANGE VALUE FOR A SMALL " & "INTEGER DECLARATION 1");
         end if;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED WHEN CHECKING SMALL" &
               " INTEGER DECLARATION 1");
      end;

      begin
         if Small_Int'Last /= 10 then
            Failed
              ("INCORRECT RANGE VALUE FOR A SMALL " & "INTEGER DECLARATION 2");
         end if;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED WHEN CHECKING SMALL" &
               " INTEGER DECLARATION 2");
      end;

      begin
         Sml := Ident_Int (3);
         if Sml = 3 then
            Comment ("VARIABLE SML OPTIMIZED VALUE 1");
         end if;
         Failed ("NO EXCEPTION RAISED FOR OUT-OF RANGE " & "VALUE 1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR OUT-OF " & "RANGE VALUE 1");
      end;

      begin
         Sml := Ident_Int (11);
         if Sml = 11 then
            Comment ("VARIABLE SML OPTIMIZED VALUE 2");
         end if;
         Failed ("NO EXCEPTION RAISED FOR OUT-OF RANGE " & "VALUE 2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR OUT-OF " & "RANGE VALUE 2");
      end;

      begin
         if Obj2'First /= Ident_Int (4) then
            Failed ("INCORRECT RANGE VALUE FOR A TYPE " & "DECLARATION 1");
         end if;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED WHEN CHECKING A " & "TYPE DECLARATION 1");
      end;

      begin
         if Obj2'Last /= Ident_Int (10) then
            Failed ("INCORRECT RANGE VALUE FOR A TYPE " & "DECLARATION 2");
         end if;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED WHEN CHECKING A " & "TYPE DECLARATION 2");
      end;

      begin
         if Ptr'First /= Ident_Int (1) then
            Failed
              ("INCORRECT RANGE VALUE FOR AN ACCESS " & "TYPE DECLARATION 1");
         end if;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED WHEN CHECKING AN " &
               "ACCESS TYPE DECLARATION 1");
      end;

      begin
         if Ptr'Last /= Ident_Int (10) then
            Failed
              ("INCORRECT RANGE VALUE FOR AN ACCESS " & "TYPE DECLARATION 2");
         end if;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED WHEN CHECKING AN " &
               "ACCESS TYPE DECLARATION 2");
      end;

      declare
         Obj_F1 : Integer range F'Range;
      begin
         Obj_F1 := Ident_Int (0);
         if Obj_F1 = 0 then
            Comment ("VARIABLE OBJ_F1 OPTIMIZED VALUE 1");
         end if;
         Failed ("NO EXCEPTION RAISED FOR OUT-OF RANGE " & "VALUE 3");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR OUT-OF " & "RANGE VALUE 3");
      end;

      declare
         Obj_F2 : Integer range F'Range;
      begin
         Obj_F2 := Ident_Int (11);
         if Obj_F2 = 11 then
            Comment ("VARIABLE OBJ_F2 OPTIMIZED VALUE 1");
         end if;
         Failed ("NO EXCEPTION RAISED FOR OUT-OF RANGE " & "VALUE 4");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED FOR OUT-OF " & "RANGE VALUE 4");
      end;
   end;
   Result;

end C36204c;
