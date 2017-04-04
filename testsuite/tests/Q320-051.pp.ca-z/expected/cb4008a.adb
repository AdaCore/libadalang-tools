-- CB4008A.ADA

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
-- CHECK THAT NESTED LAST WISHES EXCEPTION HANDLERS WORK (FOR PROCEDURES).

-- DAT 4/15/81
-- SPS 3/28/83

with Report; use Report;

procedure Cb4008a is

   C : Integer := 0;

   E : exception;

   Depth : constant := 99;

   procedure F;

   procedure I is
   begin
      C := C + 1;
      if C >= Depth then
         raise E;
      end if;
   end I;

   procedure O is
   begin
      C := C - 1;
   end O;

   procedure X is
      procedure X1 is
         procedure X2 is
         begin
            F;
         end X2;

         procedure X3 is
         begin
            I;
            X2;
         exception
            when E =>
               O;
               raise;
         end X3;
      begin
         I;
         X3;
      exception
         when E =>
            O;
            raise;
      end X1;

      procedure X1a is
      begin
         I;
         X1;
         Failed ("INCORRECT EXECUTION SEQUENCE");
      exception
         when E =>
            O;
            raise;
      end X1a;
   begin
      I;
      X1a;
   exception
      when E =>
         O;
         raise;
   end X;

   procedure Y is
   begin
      I;
      X;
   exception
      when E =>
         O;
         raise;
   end Y;

   procedure F is
      procedure F2;

      procedure F1 is
      begin
         I;
         F2;
      exception
         when E =>
            O;
            raise;
      end F1;

      procedure F2 is
      begin
         I;
         Y;
      exception
         when E =>
            O;
            raise;
      end F2;
   begin
      I;
      F1;
   exception
      when E =>
         O;
         raise;
   end F;

begin
   Test ("CB4008A", "(PROCEDURE) LAST WISHES UNWIND PROPERLY");

   begin
      I;
      Y;
      Failed ("INCORRECT EXECUTION SEQUENCE 2");
   exception
      when E =>
         O;
         if C /= 0 then
            Failed ("EXCEPTION HANDLER MISSED SOMEWHERE");
         end if;
   end;

   Result;
end Cb4008a;
