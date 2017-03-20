-- C45274C.ADA

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
-- CHECK THAT THE MEMBERSHIP OPERATOR  IN   ( NOT IN )
--     YIELDS  TRUE  (RESP.  FALSE ) IF THE DISCRIMINANTS OF THE LEFT
--     VALUE EQUAL THE DISCRIMINANTS OF THE SUBTYPE INDICATION.
--
--
--   * RECORD TYPES WITH DISCRIMINANTS;
--   * PRIVATE TYPES WITH DISCRIMINANTS;
--   * LIMITED PRIVATE TYPES WITH DISCRIMINANTS.

-- RM  3/01/82

with Report; use Report;
procedure C45274c is

begin

   Test
     ("C45274C",
      "CHECK THAT THE MEMBERSHIP OPERATOR  IN " &
      "  ( NOT IN )  YIELDS  TRUE   (RESP.  FALSE )" &
      " IF THE DISCRIMINANTS OF THE LEFT VALUE" &
      " EQUAL THE DISCRIMINANTS OF THE SUBTYPE" &
      " INDICATION");

   -------------------------------------------------------------------
   -----------------  RECORD TYPES WITH DISCRIMINANTS  ---------------

   declare

      type Rec (Discr : Boolean := False) is record
         A, B : Integer;
      end record;

      subtype Rectrue is Rec (True);

      X : Rec := (True, 19, 91);

   begin

      if X in Rectrue then
         null;
      else
         Failed ("WRONG VALUE: 'IN', 1");
      end if;

      if X not in Rectrue then
         Failed ("WRONG VALUE: 'NOT IN', 1");
      else
         null;
      end if;

   exception

      when others =>
         Failed ("1 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

   end;

   -------------------------------------------------------------------
   -----------------  PRIVATE TYPES WITH DISCRIMINANTS  --------------

   declare

      package P is
         type Priv (Discr : Boolean) is private;
      private
         type Priv (Discr : Boolean) is record
            A, B : Integer;
         end record;
      end P;

      use P;

      subtype Privtrue is Priv (Ident_Bool (True));

      X : Priv (True);

      package body P is
      begin
         X := (True, 19, 91);
      end P;

   begin

      if X in Privtrue then
         null;
      else
         Failed ("WRONG VALUE: 'IN', 2");
      end if;

      if X not in Privtrue then
         Failed ("WRONG VALUE: 'NOT IN', 2");
      else
         null;
      end if;

   exception

      when others =>
         Failed ("2 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

   end;

   -------------------------------------------------------------------
   ---------  LIMITED PRIVATE TYPES WITH DISCRIMINANTS  --------------

   declare

      package P is
         type Lp (Discr : Boolean := False) is limited private;
      private
         type Lp (Discr : Boolean := False) is record
            A, B : Integer;
         end record;
      end P;

      use P;

      subtype Lpfalse is Lp (False);

      X : Lp (True);

      package body P is
      begin
         X := (Ident_Bool (True), 19, 91);
      end P;

   begin

      if X in Lpfalse then
         Failed ("WRONG VALUE: 'IN', 3");
      else
         null;
      end if;

      if X not in Lpfalse then
         null;
      else
         Failed ("WRONG VALUE: 'NOT IN', 3");
      end if;

   exception

      when others =>
         Failed ("3 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

   end;

   -------------------------------------------------------------------

   Result;

end C45274c;
