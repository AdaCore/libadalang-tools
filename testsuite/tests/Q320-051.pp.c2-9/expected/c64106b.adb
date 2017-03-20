-- C64106B.ADA

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
-- CHECK THAT ASSIGNMENTS TO FORMAL PARAMETERS OF UNCONSTRAINED RECORD,
--   PRIVATE, AND LIMITED PRIVATE TYPES WITHOUT DEFAULT CONSTRAINTS
--   RAISE CONSTRAINT_ERROR IF AN ATTEMPT IS MADE TO CHANGE THE
--   CONSTRAINT OF THE ACTUAL PARAMETER.
--   SUBTESTS ARE:
--        (A) RECORD TYPE.
--        (B) PRIVATE TYPE.
--        (C) LIMITED PRIVATE TYPE.

-- DAS  1/15/81
-- CPP  8/9/84

with Report;
procedure C64106b is

   use Report;

begin

   Test
     ("C64106B",
      "CHECK ASSIGNMENT TO FORMAL PARAMETERS OF " &
      "UNCONSTRAINED TYPE (WITH NO DEFAULT)");

   --------------------------------------------------

   declare  -- (A)

      package Pkg is

         type Rectype (Constraint : Integer) is record
            Intfield : Integer;
            Strfield : String (1 .. Constraint);
         end record;

         procedure Chk_Rectype (Rec9 : out Rectype; Rec6 : in out Rectype);
      end Pkg;

      Rec9 : Pkg.Rectype (Ident_Int (9)) := (Ident_Int (9), 9, "123456789");
      Rec6 : Pkg.Rectype (Ident_Int (6)) := (Ident_Int (6), 5, "AEIOUY");

      package body Pkg is

         procedure Chk_Rectype (Rec9 : out Rectype; Rec6 : in out Rectype) is

            Rec4 : constant Rectype (Ident_Int (4)) :=
              (Ident_Int (4), 4, "OOPS");

         begin
            begin  -- (A.1)
               Rec9 := Rec6;
               Failed ("CONSTRAINT_ERROR NOT RAISED - A.1");
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED - A.1");
            end;   -- (A.1)

            begin  -- (A.2)
               Rec6 := Rec4;
               Failed ("CONSTRAINT_ERROR NOT RAISED - A.2");
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED - A.2");
            end;   -- (A.2)

            Rec9 := (Ident_Int (9), 9, "987654321");

         end Chk_Rectype;
      end Pkg;

   begin  -- (A)

      Pkg.Chk_Rectype (Rec9, Rec6);
      if Rec9.Strfield /= Ident_Str ("987654321") then
         Failed ("ASSIGNMENT TO REC9 FAILED - (A)");
      end if;

   end;   -- (A)

   --------------------------------------------------

   declare  -- (B)

      package Pkg is

         type Rectype (Constraint : Integer) is private;

         procedure Chk_Rectype (Rec9 : out Rectype; Rec6 : in out Rectype);
      private
         type Rectype (Constraint : Integer) is record
            Intfield : Integer;
            Strfield : String (1 .. Constraint);
         end record;
      end Pkg;

      Rec9 : Pkg.Rectype (9);
      Rec6 : Pkg.Rectype (6);

      package body Pkg is

         procedure Chk_Rectype (Rec9 : out Rectype; Rec6 : in out Rectype) is

            Rec4 : constant Rectype (4) := (4, 4, "OOPS");

         begin
            begin  -- (B.1)
               Rec9 := Rec6;
               Failed ("CONSTRAINT_ERROR NOT RAISED - B.1");
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED - B.1");
            end;   -- (B.1)

            begin  -- (B.2)
               Rec6 := Rec4;
               Failed ("CONSTRAINT_ERROR NOT RAISED - B.2");
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED - B.2");
            end;   -- (B.2)
         end Chk_Rectype;

      begin
         Rec9 := (9, 9, "123456789");
         Rec6 := (6, 5, "AEIOUY");
      end Pkg;

   begin  -- (B)

      Pkg.Chk_Rectype (Rec9, Rec6);

   end;   -- (B)

   --------------------------------------------------

   declare  -- (C)

      package Pkg is

         type Rectype (Constraint : Integer) is limited private;

         procedure Chk_Rectype (Rec9 : out Rectype; Rec6 : in out Rectype);
      private
         type Rectype (Constraint : Integer) is record
            Intfield : Integer;
            Strfield : String (1 .. Constraint);
         end record;
      end Pkg;

      Rec6 : Pkg.Rectype (Ident_Int (6));
      Rec9 : Pkg.Rectype (Ident_Int (9));

      package body Pkg is

         procedure Chk_Rectype (Rec9 : out Rectype; Rec6 : in out Rectype) is

            Rec4 : constant Rectype (4) := (4, 4, "OOPS");

         begin
            begin  -- (C.1)
               Rec9 := Rec6;
               Failed ("CONSTRAINT_ERROR NOT RAISED - C.1");
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED - C.1");
            end;   -- (C.1)

            begin  -- (C.2)
               Rec6 := Rec4;
               Failed ("CONSTRAINT_ERROR NOT RAISED - C.2");
            exception
               when Constraint_Error =>
                  null;
               when others =>
                  Failed ("WRONG EXCEPTION RAISED - C.2");
            end;   -- (C.2)
         end Chk_Rectype;

      begin
         Rec6 := (6, 5, "AEIOUY");
         Rec9 := (9, 9, "123456789");
      end Pkg;

   begin  -- (C)

      Pkg.Chk_Rectype (Rec9, Rec6);

   end;   -- (C)

   --------------------------------------------------

   Result;

end C64106b;
