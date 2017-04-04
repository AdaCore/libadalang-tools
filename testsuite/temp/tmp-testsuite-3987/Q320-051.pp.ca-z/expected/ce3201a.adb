-- CE3201A.ADA

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
-- CHECK THAT THE STANDARD INPUT AND OUTPUT FILES EXIST
-- AND ARE OPEN.

-- ABW  8/25/82
-- SPS 9/16/82
-- SPS 12/14/82
-- JBG 3/17/83

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3201a is
   Ch : Character;
begin

   Test
     ("CE3201A",
      "CHECK THAT STANDARD INPUT AND " & "OUTPUT EXIST AND ARE OPEN");

   if not Is_Open (Standard_Input) then
      Failed ("STANDARD_INPUT NOT OPEN - IS_OPEN");
   end if;

   if not Is_Open (Standard_Output) then
      Failed ("STANDARD_OUTPUT NOT OPEN - IS_OPEN");
   end if;

   begin
      Put ('X');
   exception
      when others =>
         Failed ("STANDARD_OUTPUT NOT AVAILABLE - " & "PUT DEFAULT");
   end;

   begin
      Put (Standard_Output, 'D');
   exception
      when others =>
         Failed ("STANDARD_OUTPUT NOT AVAILABLE - " & "PUT");
   end;

   Result;

end Ce3201a;
