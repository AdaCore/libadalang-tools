--C95022A.ADA

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
--CHECK THAT IT IS POSSIBLE TO ACCEPT AN ENTRY CALL FROM INSIDE THE
--THE BODY OF AN ACCEPT STATEMENT.

--CHECK THE CASE OF NORMAL ENTRY TERMINATION.

-- JEAN-PIERRE ROSEN 25-FEB-1984
-- JBG 6/1/84

-- FOUR CLIENT TASKS CALL ONE SERVER TASK.  EACH CLIENT CALLS JUST ONE
-- ENTRY OF THE SERVER TASK.  THE TEST CHECKS TO BE SURE THAT CALLS FROM
-- DIFFERENT TASKS ARE NOT MIXED UP.

with Report; use Report;
procedure C95022a is

begin
   Test
     ("C95022A",
      "CHECK THAT EMBEDDED RENDEZVOUS ARE PROCESSED " & "CORRECTLY");
   declare

      task type Client is
         entry Get_Id (I : Integer);
         entry Restart;
      end Client;

      T_Arr : array (1 .. 4) of Client;

      task Server is
         entry E1 (I : in out Integer);
         entry E2 (I : in out Integer);
         entry E3 (I : in out Integer);
         entry E4 (I : in out Integer);
      end Server;

      task body Server is
      begin

         accept E1 (I : in out Integer) do
            accept E2 (I : in out Integer) do
               I := Ident_Int (I);
               accept E3 (I : in out Integer) do
                  accept E4 (I : in out Integer) do
                     I := Ident_Int (I);
                  end E4;
                  I := Ident_Int (I);
               end E3;
            end E2;
            I := Ident_Int (I);
         end E1;

         for I in 1 .. 4 loop
            T_Arr (I).Restart;
         end loop;
      end Server;

      task body Client is
         Id      : Integer;
         Save_Id : Integer;
      begin
         accept Get_Id (I : Integer) do
            Id := I;
         end Get_Id;

         Save_Id := Id;

         case Id is
            when 1 =>
               Server.E1 (Id);
            when 2 =>
               Server.E2 (Id);
            when 3 =>
               Server.E3 (Id);
            when 4 =>
               Server.E4 (Id);
            when others =>
               Failed ("INCORRECT ID");
         end case;

         accept Restart;  -- WAIT FOR ALL TASKS TO HAVE COMPLETED
         -- RENDEZVOUS
         if Id /= Save_Id then
            Failed ("SCRAMBLED EMBEDDED RENDEZVOUS");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION IN CLIENT");
      end Client;

   begin
      for I in 1 .. 4 loop
         T_Arr (I).Get_Id (I);
      end loop;
   end;

   Result;

end C95022a;
