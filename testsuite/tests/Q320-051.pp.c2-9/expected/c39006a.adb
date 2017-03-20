-- C39006A.ADA

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
-- CHECK THAT PROGRAM_ERROR IS RAISED IF AN ATTEMPT IS MADE TO CALL A
-- SUBPROGRAM WHOSE BODY HAS NOT YET BEEN ELABORATED.  CHECK THE
-- FOLLOWING:
--     A) A FUNCTION IS CALLED IN THE INITIALIZATION EXPRESSION OF A
--        SCALAR VARIABLE OR A RECORD COMPONENT, AND THE SCALAR OR
--        RECORD VARIABLE'S DECLARATION IS ELABORATED BEFORE THE
--        SUBPROGRAM BODY IS ELABORATED.

-- TBN  8/14/86

with Report; use Report;
procedure C39006a is

begin
   Test
     ("C39006A",
      "CHECK THAT PROGRAM_ERROR IS RAISED IF AN " &
      "ATTEMPT IS MADE TO CALL A SUBPROGRAM WHOSE " &
      "BODY HAS NOT YET BEEN ELABORATED");
   begin
      declare

         function Init_1 (A : Integer) return Integer;

         Var1 : Integer := Init_1 (1);

         function Init_1 (A : Integer) return Integer is
         begin
            return (A + Ident_Int (1));
         end Init_1;

      begin
         Failed ("PROGRAM_ERROR NOT RAISED - 1");
      end;

   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 1");
   end;

   begin
      declare

         function Init_2 (A : Integer) return Integer;

         type Rec1 is record
            Number : Integer := Init_2 (2);
         end record;

         Var2 : Rec1;

         function Init_2 (A : Integer) return Integer is
         begin
            return (A + Ident_Int (1));
         end Init_2;

      begin
         Failed ("PROGRAM_ERROR NOT RAISED - 2");
      end;

   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 2");
   end;

   begin
      declare

         function F1 return Integer;

         package Pack is
            Var1 : Integer := F1;
         end Pack;

         function F1 return Integer is
         begin
            return (Ident_Int (1));
         end F1;

      begin
         Failed ("PROGRAM_ERROR NOT RAISED - 3");
      end;

   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 3");
   end;

   begin
      declare

         package Pack is
            function F2 return Integer;
            Var2 : Integer := F2;
         end Pack;

         package body Pack is
            function F2 return Integer is
            begin
               return (Ident_Int (3));
            end F2;
         end Pack;

      begin
         Failed ("PROGRAM_ERROR NOT RAISED - 4");
      end;

   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 4");
   end;

   begin
      declare

         function Init_3 (A : Integer) return Integer;

         generic
         package Q is
            Var1 : Integer := Init_3 (1);
         end Q;

         package New_Q is new Q;

         function Init_3 (A : Integer) return Integer is
         begin
            return (A + Ident_Int (3));
         end Init_3;

      begin
         Failed ("PROGRAM_ERROR NOT RAISED - 5");
      end;

   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 5");
   end;

   begin
      declare

         function Fun return Integer;

         type Param is record
            Comp : Integer := Fun;
         end record;

         generic
            type T is private;
         package Gp is
            Obj : T;
         end Gp;

         package Inst is new Gp (Param);

         function Fun return Integer is
         begin
            return (Ident_Int (3));
         end Fun;

      begin
         Failed ("PROGRAM_ERROR NOT RAISED - 6");
      end;

   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 6");
   end;

   Result;
end C39006a;
