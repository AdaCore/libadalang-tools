-- CC3606B.ADA

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
--     CHECK THAT ANY CONSTRAINTS SPECIFIED FOR THE ACTUAL
--     SUBPROGRAM'S PARAMETERS ARE USED IN PLACE OF THOSE
--     ASSOCIATED WITH THE FORMAL SUBPROGRAM'S PARAMETERS
--     (INCLUDING PARAMETERS SPECIFIED WITH A FORMAL GENERIC TYPE).

-- HISTORY:
--     LDC  06/30/88 CREATED ORIGINAL TEST.
--     PWN  05/31/96 Corrected spelling problems.

with Report; use Report;

procedure Cc3606b is

   subtype One_To_Ten is Integer range Ident_Int (1) .. Ident_Int (10);
   subtype One_To_Five is Integer range Ident_Int (1) .. Ident_Int (5);

begin
   Test
     ("CC3606B",
      "CHECK THAT ANY CONSTRAINTS SPECIFIED FOR " &
      "THE ACTUAL SUBPROGRAM'S PARAMETERS ARE USED " &
      "IN PLACE OF THOSE ASSOCIATED WITH THE " &
      "FORMAL SUBPROGRAM'S PARAMETERS (INCLUDING " &
      "PARAMETERS SPECIFIED WITH A FORMAL GENERIC " & "TYPE)");
   declare
      generic
         Brian : in out Integer;
         with procedure Passed_Proc (Lynn : in out One_To_Ten);
      package Gen is
      end Gen;

      Doug : Integer := 10;

      package body Gen is
      begin
         Passed_Proc (Brian);
         Failed ("WRONG CONSTRAINTS FOR ACTUAL PARAMETER IN GEN");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("OTHER EXCEPTION WAS RAISED FOR ACTUAL " & "PARAMETER");
      end Gen;

      procedure Proc (Jodie : in out One_To_Five) is
         John : One_To_Ten;
      begin
         John := Ident_Int (Jodie);
      exception
         when others =>
            Failed ("EXCEPTION RAISED INSIDE PROCEDURE");
      end Proc;

      package Gen_Pck is new Gen (Doug, Proc);

   begin
      null;
   end;
   declare
      type Enum is
        (Dayton, Beavercreek, Centerville, Englewood, Fairborn, Huber_Heights,
         Kettering, Miamisburg, Oakwood, Riverside, Trotwood, West_Carrollton,
         Vandalia);
      subtype Sub_Enum is Enum range Centerville .. Fairborn;

      generic
         type T_Type is (<>);
         Brian : T_Type;
         with function Passed_Func (Lynn : T_Type) return T_Type;

      package Gen_Two is
      end Gen_Two;

      Doug : Enum := Enum'First;

      package body Gen_Two is

         Dave : T_Type;

      begin
         Dave := Passed_Func (Brian);
         Failed ("WRONG CONSTRAINTS FOR ACTUAL PARAMETER IN " & "GEN_TWO");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("OTHER EXCEPTION WAS " & "RAISED FOR ACTUAL " & "PARAMETER");
      end Gen_Two;

      function Func (Jodie : Sub_Enum) return Sub_Enum is
      begin
         return Enum'Val (Ident_Int (Enum'Pos (Jodie)));
      exception
         when others =>
            Failed ("EXCEPTION RAISED INSIDE PROCEDURE");
      end Func;

      package Gen_Pck_Two is new Gen_Two (Enum, Doug, Func);

   begin
      Result;
   end;
end Cc3606b;
