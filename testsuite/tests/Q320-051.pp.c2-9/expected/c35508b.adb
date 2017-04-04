-- C35508B.ADA

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
-- CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS THE CORRECT RESULTS WHEN THE PREFIX
-- IS A GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER IS A BOOLEAN TYPE.

-- RJW 3/19/86 COMPLETELY REVISED.

with Report; use Report;

procedure C35508b is

begin

   Test
     ("C35508B",
      "CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS " &
      "THE CORRECT RESULTS WHEN THE PREFIX IS A " &
      "GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL " &
      "PARAMETER IS A BOOLEAN TYPE");

   declare
      subtype Frange is Boolean range Ident_Bool (False) .. Ident_Bool (False);
      subtype Trange is Boolean range Ident_Bool (True) .. Ident_Bool (True);
      type Newbool is new Boolean;

      generic
         type B is (<>);
         W : Integer;
      procedure P (Str : String);

      procedure P (Str : String) is
         subtype Nobool is
           B range B'Val (Ident_Int (1)) .. B'Val (Ident_Int (0));
      begin
         if B'Width /= W then
            Failed ("INCORRECT B'WIDTH FOR " & Str);
         end if;
         if Nobool'Width /= 0 then
            Failed ("INCORRECT NOBOOL'WIDTH FOR " & Str);
         end if;
      end P;

      procedure Proc1 is new P (Boolean, 5);
      procedure Proc2 is new P (Frange, 5);
      procedure Proc3 is new P (Trange, 4);
      procedure Proc4 is new P (Newbool, 5);

   begin
      Proc1 ("BOOLEAN");
      Proc2 ("FRANGE");
      Proc3 ("TRANGE");
      Proc4 ("NEWBOOL");
   end;

   Result;
end C35508b;
