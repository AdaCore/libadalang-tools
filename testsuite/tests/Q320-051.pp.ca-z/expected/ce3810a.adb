-- CE3810A.ADA

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
--     CHECK THAT FLOAT_IO PUT CAN OPERATE ON STRINGS.  ALSO CHECK THAT
--     LAYOUT_ERROR IS RAISED WHEN THE STRING IS INSUFFICIENTLY LONG.

-- HISTORY:
--     SPS 10/07/82
--     VKG 01/20/83
--     SPS 02/18/83
--     DWC 09/15/87  SPLIT CASE FOR FIXED_IO INTO CE3810B.ADA AND
--                   ADDED CASED FOR AFT AND EXP TO RAISE LAYOUT_ERROR.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3810a is
begin

   Test
     ("CE3810A",
      "CHECK THAT FLOAT_IO PUT " & "OPERATES ON STRINGS CORRECTLY");

   declare
      type Fl is digits 4;
      package Flio is new Float_Io (Fl);
      use Flio;
      St  : String (1 .. 2 + (Fl'Digits - 1) + 3 + 2);
      St1 : String (1 .. 10) := " 2.345E+02";
      St2 : String (1 .. 2);
   begin
      Put (St, 234.5);
      if St /= St1 then
         Failed ("PUT FLOAT TO STRING INCORRECT; OUTPUT WAS """ & St & """");
      end if;

      begin
         Put (St (1 .. 8), 234.5);
         Failed ("LAYOUT_ERROR NOT RAISED - FLOAT - 1");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FLOAT - 1");
      end;

      begin
         Put (St, 2.3, 9, 0);
         Failed ("LAYOUT_ERROR NOT RAISED - FLOAT - 2");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FLOAT - 2");
      end;

      begin
         Put (St2, 2.0, 0, 0);
         Failed ("LAYOUT_ERROR NOT RAISED - FLOAT - 3");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FLOAT - 3");
      end;

      begin
         Put (St, 2.345, 6, 2);
         Failed ("LAYOUT_ERROR NOT RAISED - FLOAT - 4");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FLOAT - 4");
      end;

      begin
         Put (St, 2.0, 0, 7);
         Failed ("LAYOUT_ERROR NOT RAISED - FLOAT - 5");
      exception
         when Layout_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - FLOAT - 5");
      end;
   end;

   Result;

end Ce3810a;
