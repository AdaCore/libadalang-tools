-- CE3806C.ADA

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
--     CHECK THAT PUT FOR FLOAT_IO RAISES CONSTRAINT_ERROR WHEN THE
--     VALUES SUPPLIED BY FORE, AFT, OR EXP ARE NEGATIVE OR GREATER
--     THAN FIELD'LAST WHEN FIELD'LAST < FIELD'BASE'LAST.  ALSO CHECK
--     THAT PUT FOR FLOAT_IO RAISES CONSTRAINT_ERROR WHEN THE VALUE OF
--     ITEM IS OUTSIDE THE RANGE OF THE TYPE USED TO INSTANTIATE
--     FLOAT_IO.

-- HISTORY:
--     SPS 09/10/82
--     JBG 08/30/83
--     JLH 09/14/87  ADDED CASES FOR COMPLETE OBJECTIVE.
--     KAS 11/24/95  DELETED DIGITS CONSTRAINT FROM SUBTYPE
--                   CHANGED STATIC EXPRESSIONS INVOLVING 'LAST

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3806c is

   Field_Last : Text_Io.Field := Text_Io.Field'Last;

begin

   Test
     ("CE3806C",
      "CHECK THAT PUT FOR FLOAT_IO RAISES " &
      "CONSTRAINT_ERROR APPROPRIATELY");

   declare
      type Float is digits 5 range 0.0 .. 2.0;
      subtype My_Float is Float range 0.0 .. 1.0;
      package Nfl_Io is new Float_Io (My_Float);
      use Nfl_Io;
      Ft : File_Type;
      Y  : Float    := 1.8;
      X  : My_Float := 26.3 / 26.792;

   begin
      begin
         Put (Ft, X, Fore => Ident_Int (-6));
         Failed ("CONSTRAINT_ERROR NOT RAISED - NEGATIVE FORE " & "FLOAT");
      exception
         when Constraint_Error =>
            null;
         when Status_Error =>
            Failed
              ("STATUS_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 1");
         when Use_Error =>
            Failed ("USE_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 1");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - NEGATIVE FORE " & "FLOAT");
      end;

      begin
         Put (Ft, X, Aft => Ident_Int (-2));
         Failed ("CONSTRAINT_ERROR NOT RAISED - NEGATIVE AFT " & "FLOAT");
      exception
         when Constraint_Error =>
            null;
         when Status_Error =>
            Failed
              ("STATUS_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 2");
         when Use_Error =>
            Failed ("USE_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 2");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - NEGATIVE AFT " & "FLOAT");
      end;

      begin
         Put (Ft, X, Exp => Ident_Int (-1));
         Failed ("CONSTRAINT_ERROR NOT RAISED - NEGATIVE EXP " & "FLOAT");
      exception
         when Constraint_Error =>
            null;
         when Status_Error =>
            Failed
              ("STATUS_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 3");
         when Use_Error =>
            Failed ("USE_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 3");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - NEGATIVE EXP " & "FLOAT");
      end;

      if Field_Last < Field'Base'Last then

         begin
            Put (Ft, X, Fore => Ident_Int (Field_Last + 1));
            Failed ("CONSTRAINT_ERROR NOT RAISED - FORE FLOAT");
         exception
            when Constraint_Error =>
               null;
            when Status_Error =>
               Failed
                 ("STATUS_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 4");
            when Use_Error =>
               Failed
                 ("USE_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 4");
            when others =>
               Failed ("WRONG EXCEPTION RAISED - FORE FLOAT");
         end;

         begin
            Put (Ft, X, Aft => Ident_Int (Field_Last + 1));
            Failed ("CONSTRAINT_ERROR NOT RAISED - AFT FLOAT");
         exception
            when Constraint_Error =>
               null;
            when Status_Error =>
               Failed
                 ("STATUS_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 5");
            when Use_Error =>
               Failed
                 ("USE_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 5");
            when others =>
               Failed ("WRONG EXCEPTION RAISED - AFT FLOAT");
         end;

         begin
            Put (Ft, X, Exp => Ident_Int (Field_Last + 1));
            Failed ("CONSTRAINT_ERROR NOT RAISED - EXP FLOAT");
         exception
            when Constraint_Error =>
               null;
            when Status_Error =>
               Failed
                 ("STATUS_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 6");
            when Use_Error =>
               Failed
                 ("USE_ERROR RAISED INSTEAD OF " & "CONSTRAINT_ERROR - 6");
            when others =>
               Failed ("WRONG EXCEPTION RAISED - EXP FLOAT");
         end;
      end if;

      begin
         Put (Ft, Y);
         Failed
           ("CONSTRAINT_ERROR NOT RAISED FOR ITEM OUTSIDE " & "RANGE - FILE");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED FOR ITEM OUTSIDE " & "RANGE - FILE");
      end;

      begin
         Put (Y);
         Failed
           ("CONSTRAINT_ERROR NOT RAISED FOR ITEM OUTSIDE " &
            "RANGE - DEFAULT");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED FOR ITEM OUTSIDE " & "RANGE - DEFAULT");
      end;

   end;

   Result;

end Ce3806c;
