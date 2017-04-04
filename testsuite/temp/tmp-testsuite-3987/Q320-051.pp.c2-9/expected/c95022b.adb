-- C95022B.ADA

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
-- CHECK THAT IT IS POSSIBLE TO ACCEPT AN ENTRY CALL FROM INSIDE
-- THE BODY OF AN ACCEPT STATEMENT.

-- CHECK THE CASE OF ABORT DURING THE INNERMOST ACCEPT.

-- JEAN-PIERRE ROSEN 25-FEB-1984
-- JBG 6/1/84

with Report; use Report;
procedure C95022b is

begin

   Test
     ("C95022B",
      "CHECK THAT EMBEDDED RENDEZVOUS ARE PROCESSED " &
      "CORRECTLY (ABORT CASE)");
   declare
      task type Client is
         entry Get_Id (I : Integer);
      end Client;

      T_Arr : array (1 .. 4) of Client;

      task Kill is
         entry Me;
      end Kill;

      task Server is
         entry E1;
         entry E2;
         entry E3;
         entry E4;
      end Server;

      task body Server is
      begin

         accept E1 do
            accept E2 do
               accept E3 do
                  accept E4 do
                     Kill.Me;
                     E1;  -- WILL DEADLOCK UNTIL ABORT.
                  end E4;
               end E3;
            end E2;
         end E1;

      end Server;

      task body Kill is
      begin
         accept Me;
         abort Server;
      end Kill;

      task body Client is
         Id : Integer;
      begin
         accept Get_Id (I : Integer) do
            Id := I;
         end Get_Id;

         case Id is
            when 1 =>
               Server.E1;
            when 2 =>
               Server.E2;
            when 3 =>
               Server.E3;
            when 4 =>
               Server.E4;
            when others =>
               Failed ("INCORRECT ID");
         end case;

         Failed ("TASKING_ERROR NOT RAISED IN CLIENT" & Integer'Image (Id));

      exception
         when Tasking_Error =>
            null;
         when others =>
            Failed ("EXCEPTION IN CLIENT" & Integer'Image (Id));
      end Client;
   begin
      for I in 1 .. 4 loop
         T_Arr (I).Get_Id (I);
      end loop;
   end;

   Result;

end C95022b;
