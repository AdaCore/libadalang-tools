-- C45274B.ADA

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
-- CHECK THAT THE MEMBERSHIP OPERATOR  IN   ( NOT IN )  ALWAYS
--     YIELDS  TRUE   (RESP.  FALSE )  FOR
--
--   * RECORD TYPES WITHOUT DISCRIMINANTS;
--   * PRIVATE TYPES WITHOUT DISCRIMINANTS;
--   * LIMITED PRIVATE TYPES WITHOUT DISCRIMINANTS;
-->> * (UNCONSTRAINED) RECORD TYPES WITH DISCRIMINANTS;
-->> * (UNCONSTRAINED) PRIVATE TYPES WITH DISCRIMINANTS;
-->> * (UNCONSTRAINED) LIMITED PRIVATE TYPES WITH DISCRIMINANTS.

-- RM  3/03/82

with Report; use Report;
procedure C45274b is

begin

   Test
     ("C45274B",
      "CHECK THAT THE MEMBERSHIP OPERATOR  IN " &
      "  ( NOT IN )  YIELDS  TRUE   (RESP.  FALSE )" &
      " FOR UNCONSTRAINED TYPES WITH DISCRIMINANTS");

   -------------------------------------------------------------------
   --------  UNCONSTRAINED RECORD TYPES WITH DISCRIMINANTS  ----------

   declare

      type Rec (Discr : Boolean) is record
         A, B : Integer;
      end record;

      X : Rec (False) := (False, 19, 81);

      type Rec0 (Discr : Boolean := False) is record
         A, B : Integer;
      end record;

      Y : Rec0 := (True, 19, 81);

   begin

      if X in Rec then
         null;
      else
         Failed ("WRONG VALUE: 'IN', 1A");
      end if;

      if Y not in Rec0 then
         Failed ("WRONG VALUE: 'NOT IN', 1B");
      else
         null;
      end if;

   exception

      when others =>
         Failed ("1 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

   end;

   -------------------------------------------------------------------
   -------  UNCONSTRAINED PRIVATE TYPES WITH DISCRIMINANTS  ----------

   declare

      package P is
         type Priv (Discr : Boolean) is private;
      private
         type Priv (Discr : Boolean) is record
            A, B : Integer;
         end record;
      end P;

      use P;

      X : Priv (False);

      package body P is
      begin
         X := (False, 19, 91);
      end P;

   begin

      if X in Priv then
         null;
      else
         Failed ("WRONG VALUE: 'IN', 2");
      end if;

      if X not in Priv then
         Failed ("WRONG VALUE: 'NOT IN', 2");
      else
         null;
      end if;

   exception

      when others =>
         Failed ("2 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

   end;

   -------------------------------------------------------------------
   ---------  UNCONSTRAINED LIM. PRIV. TYPES WITH DISCRIM.  ----------

   declare

      package P is
         type Lp (Discr : Boolean := False) is limited private;
      private
         type Lp (Discr : Boolean := False) is record
            A, B : Integer;
         end record;
      end P;

      use P;

      X : Lp (True);

      package body P is
      begin
         X := (True, 19, 91);
      end P;

   begin

      if X in Lp then
         null;
      else
         Failed ("WRONG VALUE: 'IN', 3");
      end if;

      if X not in Lp then
         Failed ("WRONG VALUE: 'NOT IN', 3");
      else
         null;
      end if;

   exception

      when others =>
         Failed ("3 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

   end;

   -------------------------------------------------------------------

   declare

      package P is
         type Lp (Discr : Boolean := False) is limited private;
      private
         type Lp (Discr : Boolean := False) is record
            A, B : Integer;
         end record;
      end P;

      use P;

      Y : Lp (True);

   -- CHECK THAT NO EXCEPTION FOR UNINITIALIZED VARIABLE
   begin

      if Y in Lp then
         null;
      else
         Failed ("WRONG VALUE: 'IN', 3BIS");
      end if;

      if Y not in Lp then
         Failed ("WRONG VALUE: 'NOT IN', 3BIS");
      else
         null;
      end if;

   exception

      when others =>
         Failed
           ("3BIS - UNINITIALIZED VARIABLE - 'IN' " &
            "( 'NOT IN' )  RAISED AN EXCEPTION");

   end;

   -------------------------------------------------------------------

   Result;

end C45274b;
