-- CE3706C.ADA

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
--     CHECK THAT INTEGER_IO PUT RAISES CONSTRAINT_ERROR IF:
--        A) THE BASE IS OUTSIDE THE RANGE 2..16.
--        B) THE VALUE OF WIDTH IS NEGATIVE OR GREATER THAN FIELD'LAST,
--           WHEN FIELD'LAST < INTEGER'LAST.
--        C) THE VALUE OF ITEM IS OUTSIDE THE RANGE OF THE INSTANTIATED
--           TYPE.

-- HISTORY:
--     SPS 10/05/82
--     JBG 08/30/83
--     JLH 09/10/87  ADDED CASES FOR THE VALUE OF THE WIDTH BEING LESS
--                   THAN ZERO AND GREATER THAN FIELD'LAST AND CASES FOR
--                   THE VALUE OF ITEM OUTSIDE THE RANGE OF THE
--                   INSTANTIATED TYPE.
--     JRL 06/07/96  Added call to Ident_Int in expressions involving
--                   Field'Last, to make the expressions non-static and
--                   prevent compile-time rejection.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3706c is
begin

   Test
     ("CE3706C",
      "CHECK THAT INTEGER_IO PUT RAISES CONSTRAINT " & "ERROR APPROPRIATELY");

   declare
      Ft : File_Type;
      type Int is new Integer range 1 .. 10;
      package Iio is new Integer_Io (Int);
      use Iio;
      St : String (1 .. 10);
   begin

      begin
         Put (Ft, 2, 6, 1);
         Failed ("CONSTRAINT_ERROR NOT RAISED - FILE - 1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FILE - 1");
      end;

      begin
         Put (3, 4, 17);
         Failed ("CONSTRAINT_ERROR NOT RAISED - DEFAULT - 1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - DEFAULT - 1");
      end;

      begin
         Put (To => St, Item => 4, Base => -3);
         Failed ("CONSTRAINT_ERROR NOT RAISED - STRING - 1");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - STRING - 1");
      end;

      begin
         Put (St, 5, 17);
         Failed ("CONSTRAINT_ERROR NOT RAISED - STRING - 2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - STRING - 2");
      end;

      begin
         Put (Ft, 5, -1);
         Failed ("CONSTRAINT_ERROR NOT RAISED - FILE - 2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FILE - 2");
      end;

      begin
         Put (7, -3);
         Failed ("CONSTRAINT_ERROR NOT RAISED - DEFAULT - " & "2");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - DEFAULT - 2");
      end;

      if Field'Last < Integer'Last then
         begin
            Put (7, Field'Last + Ident_Int (1));
            Failed
              ("CONSTRAINT_ERROR NOT RAISED FOR WIDTH " &
               "GREATER THAN FIELD'LAST");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED FOR WIDTH " &
                  "GREATER THAN FIELD'LAST");
         end;

      end if;

      begin
         Put (Ft, 11);
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
         Put (11);
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
end Ce3706c;
