-- CB4007A.ADA

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
--     CHECK THAT THE STATEMENT PART OF A PACKAGE CAN RAISE, PROPAGATE,
--     AND HANDLE EXCEPTIONS. IF THE BODY'S HANDLERS HANDLE ALL
--     EXCEPTIONS RAISED AND DO NOT RAISE ANY UNHANDLED EXCEPTIONS,
--     NO EXCEPTION IS PROPAGATED.

-- HISTORY:
--     DHH 03/28/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cb4007a is
begin

   Test
     ("CB4007A",
      "CHECK THAT THE STATEMENT PART OF A PACKAGE " &
      "CAN RAISE, PROPAGATE, AND HANDLE EXCEPTIONS. " &
      "IF THE BODY'S HANDLERS HANDLE ALL EXCEPTIONS " &
      "RAISED AND DO NOT RAISE ANY UNHANDLED " &
      "EXCEPTIONS, NO EXCEPTION IS PROPAGATED");
   declare

      package Outside is
      end Outside;

      package body Outside is

      begin
         declare
            package Handler is
            end Handler;

            package body Handler is
            begin
               declare
                  package Propagate is
                  end Propagate;

                  package body Propagate is
                  begin
                     declare
                        package Rise is
                        end Rise;

                        package body Rise is
                        begin
                           raise Constraint_Error;
                           Failed ("EXCEPTION " & "NOT RAISED");
                        end Rise;

                     begin
                        null;
                     end;   -- PACKAGE PROPAGATE DECLARE.
                  exception
                     when Constraint_Error =>
                        raise Constraint_Error;
                     when others =>
                        Failed
                          ("UNEXPECTED EXCEPTION " &
                           "RAISED IN PROPAGATE " &
                           "PACKAGE");
                  end Propagate;

               begin
                  null;
               end;               -- PACKAGE HANDLER DECLARE.
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed
                    ("UNEXPECTED EXCEPTION RAISED IN " & "HANDLER PACKAGE");
            end Handler;

         begin
            null;
         end;                    -- PACKAGE OUTSIDE DECLARE.
      exception
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED IN OUTSIDE " & "PACKAGE");
      end Outside;
   begin
      null;
   end;

   Result;

exception
   when others =>
      Failed ("UNEXPECTED EXCEPTION RAISED");
      Result;
end Cb4007a;
