-- CA2009F0M.ADA

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
--     CHECK THAT A GENERIC SUBPROGRAM SUBUNIT CAN BE SPECIFIED AND
--     INSTANTIATED.  IN THIS TEST, SOME SUBUNIT BODIES ARE
--     IN SEPARATE FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST MUST RUN AND REPORT "PASSED" FOR ALL ADA 95 IMPLEMENTATIONS.

-- SEPARATE FILES ARE:
--     CA2009F0M  THE MAIN PROCEDURE, WITH SUBUNIT BODIES FOR
--                     PROC2 AND FUNC2.
--     CA2009F1   A SUBUNIT PROCEDURE BODY (PROC1).
--     CA2009F2   A SUBUNIT FUNCTION BODY  (FUNC1).

-- HISTORY:
--     BHS 08/01/84  CREATED ORIGINAL TEST.
--     PWB 02/19/86  ADDED "SOME" TO FIRST COMMENT.
--     BCB 01/05/88  MODIFIED HEADER.
--     EDS 08/04/98  REMOVE CONTROL Z AT END OF FILE.
--     RLB 09/13/99  UPDATED APPLICABILITY CRITERIA FOR ADA 95.
--     RLB 09/15/99  REMOVED JUNK COMMENT.

with Report; use Report;
procedure Ca2009f0m is

   Int1 : Integer := 1;
   Int2 : Integer := 2;
   Int3 : Integer := 3;
   Int4 : Integer := 4;

   generic
      type Elem is private;
      Pcon1 : in Elem;
      Pvar1 : in out Elem;
   procedure Proc1;

   generic
      type Elem is private;
      Pcon2 : in Elem;
      Pvar2 : in out Elem;
   procedure Proc2;

   generic
      type Obj is private;
      Fcon1 : in Obj;
      Fvar1 : in out Obj;
   function Func1 return Obj;

   generic
      type Obj is private;
      Fcon2 : in Obj;
      Fvar2 : in out Obj;
   function Func2 return Obj;

   procedure Proc1 is separate;
   procedure Proc2 is separate;
   function Func1 return Obj is separate;
   function Func2 return Obj is separate;

   procedure Ni_Proc1 is new Proc1 (Integer, 2, Int1);
   procedure Ni_Proc2 is new Proc2 (Integer, 3, Int2);
   function Ni_Func1 is new Func1 (Integer, 4, Int3);
   function Ni_Func2 is new Func2 (Integer, 5, Int4);

begin

   Test
     ("CA2009F",
      "SPECIFICATION AND INSTANTIATION " & "OF GENERIC SUBPROGRAM SUBUNITS");

   Ni_Proc1;
   if Int1 /= 2 then
      Failed ("INCORRECT INSTANTIATION - NI_PROC1");
   end if;

   Ni_Proc2;
   if Int2 /= 3 then
      Failed ("INCORRECT INSTANTIATION - NI_PROC2");
   end if;

   if Ni_Func1 /= 4 then
      Failed ("INCORRECT INSTANTIATION - NI_FUNC1");
   end if;

   if Ni_Func2 /= 5 then
      Failed ("INCORRECT INSTANTIATION - NI_FUNC2");
   end if;

   Result;

end Ca2009f0m;
