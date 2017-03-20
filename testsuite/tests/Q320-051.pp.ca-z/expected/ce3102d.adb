-- CE3102D.ADA

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
--     CHECK THAT STATUS_ERROR IS RAISED BY CLOSE, DELETE, RESET, MODE,
--     NAME, AND FORM IF THE GIVEN TEXT FILES ARE NOT OPEN.

-- HISTORY:
--     JLH 08/10/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3102d is

   Incomplete : exception;
   File : File_Type;
   Ft   : File_Type;

begin

   Test
     ("CE3102D",
      "CHECK THAT STATUS_ERROR IS RAISED " & "APPROPRIATELY FOR TEXT FILES");

   begin
      Create (Ft);
      Close (Ft);
   exception
      when Use_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR CREATE");
   end;

   begin
      Reset (Ft);
      Failed ("STATUS_ERROR NOT RAISED FOR RESET");
   exception
      when Status_Error =>
         null;
      when Use_Error =>
         Failed ("USE_ERROR RAISED FOR RESET OF CLOSED FILE");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR RESET");
   end;

   begin
      declare
         Md : File_Mode := Mode (Ft);
      begin
         Failed ("STATUS_ERROR NOT RAISED FOR MODE");
      end;
   exception
      when Status_Error =>
         null;
      when Use_Error =>
         Failed ("USE_ERROR RAISED FOR MODE OF CLOSED FILE");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR MODE");
   end;

   begin
      declare
         Nm : constant String := Name (Ft);
      begin
         Failed ("STATUS_ERROR NOT RAISED FOR NAME");
      end;
   exception
      when Status_Error =>
         null;
      when Use_Error =>
         Failed ("USE_ERROR RAISED FOR NAME OF CLOSED FILE");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR NAME");
   end;

   begin
      declare
         Fm : constant String := Form (Ft);
      begin
         Failed ("STATUS_ERROR NOT RAISED FOR FORM");
      end;
   exception
      when Status_Error =>
         null;
      when Use_Error =>
         Failed ("USE_ERROR RAISED FOR FORM OF CLOSED FILE");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR FORM");
   end;

   begin
      Close (Ft);
      Failed ("STATUS_ERROR NOT RAISED FOR CLOSE");
   exception
      when Status_Error =>
         null;
      when Use_Error =>
         Failed ("USE_ERROR RAISED WHEN CLOSING CLOSED FILE");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR CLOSE");
   end;

   begin
      Delete (Ft);
      Failed ("STATUS_ERROR NOT RAISED FOR DELETE");
   exception
      when Status_Error =>
         null;
      when Use_Error =>
         Failed ("USE_ERROR RAISED FOR DELETE OF CLOSED FILE");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR DELETE");
   end;

   Result;

exception
   when Incomplete =>
      Result;

end Ce3102d;
