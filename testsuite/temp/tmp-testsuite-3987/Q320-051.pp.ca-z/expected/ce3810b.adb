-- CE3810B.ADA

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
--     CHECK THAT FIXED_IO PUT CAN OPERATE ON STRINGS.  ALSO CHECK THAT
--     LAYOUT_ERROR IS RAISED WHEN THE STRING IS INSUFFICIENTLY LONG.

-- HISTORY:
--     DWC 09/15/87  CREATE ORIGINAL TEST.
--     JRL 02/28/96  Changed upper bound of type FX from 1000.0 to 250.0.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3810b is
begin

   Test
     ("CE3810B",
      "CHECK THAT FIXED_IO PUT CAN OPERATE ON " &
      "STRINGS.  ALSO CHECK THAT LAYOUT_ERROR IS " &
      "RAISED WHEN THE STRING IS INSUFFICIENTLY LONG");

   declare
      type Fx is delta 0.000_1 range 0.0 .. 250.0;
      package Fxio is new Fixed_Io (Fx);
      use Fxio;
      St1 : constant String := "  234.5000";
      St  : String (St1'Range);
      St2 : String (1 .. 2);

   begin
      begin
         Put (St, 234.5);
      exception
         when Layout_Error =>
            Failed ("LAYOUT_ERROR RAISED ON PUT" & "TO STRING - FIXED");
         when others =>
            Failed ("SOME EXCEPTION RAISED ON PUT" & "TO STRING -FIXED");
      end;

      if St /= St1 then
         Failed
           ("PUT FIXED TO STRING INCORRECT; OUTPUT " & "WAS """ & St & """");
      end if;

      begin
         Put (St (1 .. 7), 234.500_0);
         Failed ("LAYOUT_ERROR NOT RAISED - FIXED - 1");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED - 1");
      end;

      begin
         Put (St, 2.3, 9, 0);
         Failed ("LAYOUT_ERROR NOT RAISED - FIXED - 2");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED - 2");
      end;

      begin
         Put (St2, 2.0, 0, 0);
         Failed ("LAYOUT_ERROR NOT RAISED - FIXED - 3");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED - 3");
      end;

      begin
         Put (St, 2.345, 6, 2);
         Failed ("LAYOUT_ERROR NOT RAISED - FIXED - 4");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED - 4");
      end;

      begin
         Put (St, 2.0, 0, 7);
         Failed ("LAYOUT_ERROR NOT RAISED - FIXED - 5");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FIXED - 5");
      end;
   end;

   Result;
end Ce3810b;
