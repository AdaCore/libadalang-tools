-- CD2B16A.ADA

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
--     IF A COLLECTION SIZE CLAUSE IS GIVEN FOR A PARENT ACCESS TYPE,
--     THEN THE DERIVED TYPE HAS THE SAME COLLECTION SIZE, WHETHER THE
--     DERIVED TYPE IS DECLARED BEFORE OR AFTER THE PARENT COLLECTION
--     SIZE SPECIFICATION.

-- HISTORY:
--     DHH 09/29/87 CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with System;
with Report; use Report;
procedure Cd2b16a is
begin
   Test
     ("CD2B16A",
      "IF A COLLECTION SIZE IS GIVEN FOR A " &
      "PARENT ACCESS TYPE, THEN THE DERIVED TYPE HAS " &
      "THE SAME COLLECTION SIZE, WHETHER THE " &
      "DERIVED TYPE IS DECLARED BEFORE OR AFTER " &
      "THE PARENT COLLECTION SIZE SPECIFICATION");

   declare

      Collection_Size : constant := 128;
      type V is array (1 .. 4) of Integer;

      type Cell is record
         Value : V;
      end record;

      type Link is access Cell;
      type Newlink1 is new Link;

      for Link'Storage_Size use Collection_Size;

      type Newlink2 is new Link;

   begin    -- ACTIVE DECLARE

      if Link'Storage_Size < Collection_Size then
         Failed
           ("STORAGE_SIZE SMALLER THAN STORAGE_SIZE " &
            "SPECIFIED WAS ALLOCATED");
      end if;

      if Link'Storage_Size /= Newlink1'Storage_Size then
         Failed
           ("STORAGE_SIZE OF THE FIRST DERIVED TYPE" &
            "IS NOT THE SAME SIZE AS THAT OF THE " &
            "PARENT");
      end if;

      if Link'Storage_Size /= Newlink2'Storage_Size then
         Failed
           ("STORAGE_SIZE OF THE SECOND DERIVED TYPE" &
            "IS NOT THE SAME SIZE AS THAT OF THE " &
            "PARENT");
      end if;

   end;    --ACTIVE DECLARE

   Result;
end Cd2b16a;
