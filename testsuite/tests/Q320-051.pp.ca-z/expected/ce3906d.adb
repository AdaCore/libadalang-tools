-- CE3906D.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED BY PUT FOR ENUMERATION
--     TYPES WHEN THE VALUE OF WIDTH IS NEGATIVE, WHEN WIDTH IS
--     GREATER THAN FIELD'LAST, OR WHEN THE VALUE OF ITEM IS OUTSIDE
--     THE RANGE OF THE SUBTYPE USED TO INSTANTIATE ENUMERATION_IO.

-- HISTORY:
--     SPS 10/08/82
--     DWC 09/17/87  ADDED CASES FOR CONSTRAINT_ERROR.
--     JRL 06/07/96  Added call to Ident_Int in expressions involving
--                   Field'Last, to make the expressions non-static and
--                   prevent compile-time rejection.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3906d is
begin

   Test
     ("CE3906D",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED BY PUT " &
      "FOR ENUMERATION TYPES WHEN THE VALUE OF " &
      "WIDTH IS NEGATIVE, WHEN WIDTH IS GREATER " &
      "THAN FIELD'LAST, OR WHEN THE VALUE OF ITEM " &
      "IS OUTSIDE THE RANGE OF THE SUBTYPE USED TO " &
      "INSTANTIATE ENUMERATION_IO");

   declare
      Ft : File_Type;
      type Day is
        (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
      Today : Day := Friday;
      subtype Weekday is Day range Monday .. Friday;
      package Day_Io is new Enumeration_Io (Weekday);
      use Day_Io;
   begin

      begin
         Put (Ft, Today, -1);
         Failed ("CONSTRAINT_ERROR NOT RAISED; NEGATIVE " & "WIDTH - FILE");
      exception
         when Constraint_Error =>
            null;
         when Status_Error =>
            Failed ("RAISED STATUS_ERROR");
         when others =>
            Failed ("WRONG EXCEPTION RAISED; NEGATIVE " & "WIDTH - FILE");
      end;

      if Field'Last < Integer'Last then
         begin
            Put (Ft, Today, Field'Last + Ident_Int (1));
            Failed
              ("CONSTRAINT_ERROR NOT RAISED; WIDTH " &
               "GREATER THAN FIELD'LAST + 1- FILE");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED; WIDTH " &
                  "GREATER THAN FIELD'LAST + 1 - FILE");
         end;

         begin
            Put (Today, Field'Last + Ident_Int (1));
            Failed
              ("CONSTRAINT_ERROR NOT RAISED; WIDTH " &
               "GREATER THAN FIELD'LAST + 1 - DEFAULT");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED; WIDTH " &
                  "GREATER THAN FIELD'LAST + 1 " & "- DEFAULT");
         end;

      end if;

      Today := Saturday;

      begin
         Put (Ft, Today);
         Failed
           ("CONSTRAINT_ERROR NOT RAISED; ITEM VALUE " &
            "OUT OF RANGE - FILE");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED; ITEM VALUE " & "OUT OF RANGE - FILE");
      end;

      Today := Friday;

      begin
         Put (Today, -3);
         Failed ("CONSTRAINT_ERROR NOT RAISED; NEGATIVE " & "WIDTH - DEFAULT");
      exception
         when Constraint_Error =>
            null;
         when Status_Error =>
            Failed ("RAISED STATUS_ERROR");
         when others =>
            Failed ("WRONG EXCEPTION RAISED; NEGATIVE " & "WIDTH - DEFAULT");
      end;

      Today := Saturday;

      begin
         Put (Today);
         Failed
           ("CONSTRAINT_ERROR NOT RAISED; ITEM VALUE " &
            "OUT OF RANGE - DEFAULT");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED; ITEM VALUE " &
               "OUT OF RANGE - DEFAULT");
      end;
   end;

   Result;

end Ce3906d;
