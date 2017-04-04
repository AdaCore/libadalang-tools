-- C45274A.ADA

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
-- CHECK THAT THE MEMBERSHIP OPERATOR IN ( NOT IN ) ALWAYS
--     YIELDS  TRUE   (RESP.  FALSE )  FOR
--
-->> * RECORD TYPES WITHOUT DISCRIMINANTS;
-->> * PRIVATE TYPES WITHOUT DISCRIMINANTS;
-->> * LIMITED PRIVATE TYPES WITHOUT DISCRIMINANTS;
--   * (UNCONSTRAINED) RECORD TYPES WITH DISCRIMINANTS;
--   * (UNCONSTRAINED) PRIVATE TYPES WITH DISCRIMINANTS;
--   * (UNCONSTRAINED) LIMITED PRIVATE TYPES WITH DISCRIMINANTS.

-- RM  3/01/82

with Report; use Report;
procedure C45274a is

begin

   Test
     ("C45274A",
      "CHECK THAT THE MEMBERSHIP OPERATOR  IN " &
      "  ( NOT IN )  YIELDS  TRUE   (RESP.  FALSE )" &
      " FOR RECORD TYPES WITHOUT DISCRIMINANTS," &
      " PRIVATE TYPES WITHOUT DISCRIMINANTS, AND" &
      " LIMITED PRIVATE TYPES WITHOUT DISCRIMINANTS");

   -------------------------------------------------------------------
   -----------------  RECORD TYPES WITHOUT DISCRIMINANTS  ------------

   declare

      type Rec is record
         A, B : Integer;
      end record;

      X : Rec := (19, 91);

   begin

      if X in Rec then
         null;
      else
         Failed ("WRONG VALUE: 'IN', 1");
      end if;

      if X not in Rec then
         Failed ("WRONG VALUE: 'NOT IN', 1");
      else
         null;
      end if;

   exception

      when others =>
         Failed ("1 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

   end;

   -------------------------------------------------------------------
   -----------------  PRIVATE TYPES WITHOUT DISCRIMINANTS  -----------

   declare

      package P is
         type Priv is private;
      private
         type Priv is record
            A, B : Integer;
         end record;
      end P;

      use P;

      X : Priv;

      package body P is
      begin
         X := (19, 91);
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
   ---------  LIMITED PRIVATE TYPES WITHOUT DISCRIMINANTS  -----------

   declare

      package P is
         type Lp is limited private;
      private
         type Lp is record
            A, B : Integer;
         end record;
      end P;

      use P;

      X : Lp;

      package body P is
      begin
         X := (19, 91);
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
         type Lp is limited private;
      private
         type Lp is record
            A, B : Integer;
         end record;
      end P;

      use P;

      Y : Lp;

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

end C45274a;
