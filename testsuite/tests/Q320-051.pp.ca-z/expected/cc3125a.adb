-- CC3125A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE INITIAL VALUE OF A GENERIC IN
-- PARAMETER DOES NOT SATISFY ITS SUBTYPE CONSTRAINT.

-- THIS TEST CHECKS PARAMETERS OF A NON-GENERIC TYPE.

-- DAT 8/10/81
-- SPS 4/14/82

with Report; use Report;

procedure Cc3125a is

begin
   Test
     ("CC3125A",
      "GENERIC PARAMETER DEFAULTS OF " &
      "NON-GENERIC TYPE EVALUATED AND CHECKED WHEN " &
      "DECLARATION IS INSTANTIATED AND DEFAULT USED");

   for I in 1 .. 3 loop
      Comment ("LOOP ITERATION");
      begin

         declare
            subtype T is Integer range 1 .. Ident_Int (1);
            subtype I_1_2 is Integer range Ident_Int (1) .. Ident_Int (2);

            generic
               P, Q : T := I_1_2'(I);
            package Pkg is
               R : T := P;
            end Pkg;

         begin

            begin
               declare
                  package P1 is new Pkg;
               begin
                  if I = Ident_Int (1) then
                     if P1.R /= Ident_Int (1) then
                        Failed ("BAD INITIAL" & " VALUE");
                     end if;
                  elsif I = 2 then
                     Failed ("SUBTYPE NOT CHECKED AT " & "INSTANTIATION");
                  else
                     Failed ("DEFAULT NOT EVALUATED AT " & "INSTANTIATION");
                  end if;
               exception
                  when others =>
                     Failed ("WRONG HANDLER");
               end;
            exception
               when Constraint_Error =>
                  case I is
                     when 1 =>
                        Failed ("INCORRECT EXCEPTION");
                     when 2 =>
                        Comment ("CONSTRAINT CHECKED" & " ON INSTANTIATION");
                     when 3 =>
                        Comment ("DEFAULT EVALUATED " & "ON INSTANTIATION");
                  end case;
            end;
         exception
            when others =>
               Failed ("WRONG EXCEPTION");
         end;
      exception
         when Constraint_Error =>
            case I is
               when 1 =>
                  Failed ("NO EXCEPTION SHOULD BE RAISED");
               when 2 =>
                  Failed
                    ("DEFAULT CHECKED AGAINST " & "SUBTYPE AT DECLARATION");
               when 3 =>
                  Failed ("DEFAULT EVALUATED AT " & "DECLARATION");
            end case;
      end;
   end loop;

   Result;
end Cc3125a;
