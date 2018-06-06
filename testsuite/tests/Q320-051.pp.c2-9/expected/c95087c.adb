-- C95087C.ADA

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
-- CHECK THAT ASSIGNMENTS TO ENTRY FORMAL PARAMETERS OF UNCONSTRAINED
--    RECORD, PRIVATE, AND LIMITED PRIVATE TYPES WITH DEFAULT
--    CONSTRAINTS RAISE CONSTRAINT_ERROR IF THE ACTUAL PARAMETER IS
--    CONSTRAINED AND THE CONSTRAINT VALUES OF THE OBJECT BEING
--    ASSIGNED TO DO NOT SATISFY THOSE OF THE ACTUAL PARAMETER.

--    SUBTESTS ARE:
--        (A) CONSTRAINED ACTUAL PARAMETERS OF RECORD TYPE.
--        (B) CONSTRAINED ACTUAL PARAMETERS OF PRIVATE TYPE.
--        (C) CONSTRAINED ACTUAL PARAMETERS OF LIMITED PRIVATE TYPE.

-- RJW  1/15/86

with Report; use Report;
procedure C95087c is

begin

   Test
     ("C95087C",
      "CHECK ASSIGNMENTS TO ENTRY FORMAL " &
      "PARAMETERS OF UNCONSTRAINED TYPES " & "(WITH DEFAULTS)");

   --------------------------------------------------

   declare  -- (A)

      package Pkg is

         subtype Intrange is Integer range 0 .. 31;

         type Rectype (Constraint : Intrange := 15) is record
            Intfld : Intrange;
            Strfld : String (1 .. Constraint);
         end record;

         Rec91, Rec92, Rec93 : Rectype (9);
         Rec_Oops            : Rectype (4);

         task T is
            entry E
              (Rec1 : in Rectype; Rec2 : in out Rectype; Rec3 : out Rectype);
         end T;

      end Pkg;

      package body Pkg is

         task body T is
         begin
            accept E
              (Rec1 : in Rectype; Rec2 : in out Rectype; Rec3 : out Rectype) do

               if (not Rec1'Constrained) or (Rec1.Constraint /= Ident_Int (9))
               then
                  Failed
                    ("CONSTRAINT ON RECORD TYPE " &
                     "IN PARAMETER NOT RECOGNIZED");
               end if;

               begin  -- ASSIGNMENT TO IN OUT PARAMETER.
                  Rec2 := Rec_Oops;
                  Failed ("CONSTRAINT_ERROR NOT " & "RAISED - A.1");
               exception
                  when Constraint_Error =>
                     null;
                  when others =>
                     Failed ("WRONG EXCEPTION " & "RAISED - A.1");
               end;

               begin  -- ASSIGNMENT TO OUT PARAMETER.
                  Rec3 := Rec_Oops;
                  Failed ("CONSTRAINT_ERROR NOT " & "RAISED - A.2");
               exception
                  when Constraint_Error =>
                     null;
                  when others =>
                     Failed ("WRONG EXCEPTION " & "RAISED - A.2");
               end;

            end E;
         end T;

      begin

         Rec91 := (9, 9, "123456789");
         Rec92 := Rec91;
         Rec93 := Rec91;

         Rec_Oops := (4, 4, "OOPS");

      end Pkg;

   begin  -- (A)

      Pkg.T.E (Pkg.Rec91, Pkg.Rec92, Pkg.Rec93);

   end;   -- (A)

   --------------------------------------------------

   declare  -- (B)

      package Pkg is

         subtype Intrange is Integer range 0 .. 31;

         type Rectype (Constraint : Intrange := 15) is private;

         task T is
            entry E
              (Rec1 : in Rectype; Rec2 : in out Rectype; Rec3 : out Rectype);
         end T;

      private

         type Rectype (Constraint : Intrange := 15) is record
            Intfld : Intrange;
            Strfld : String (1 .. Constraint);
         end record;
      end Pkg;

      Rec91, Rec92, Rec93 : Pkg.Rectype (9);
      Rec_Oops            : Pkg.Rectype (4);

      package body Pkg is

         task body T is
         begin
            accept E
              (Rec1 : in Rectype; Rec2 : in out Rectype; Rec3 : out Rectype) do

               if (not Rec1'Constrained) or (Rec1.Constraint /= Ident_Int (9))
               then
                  Failed
                    ("CONSTRAINT ON PRIVATE TYPE " &
                     "IN PARAMETER NOT RECOGNIZED");
               end if;

               begin  -- ASSIGNMENT TO IN OUT PARAMETER.
                  Rec2 := Rec_Oops;
                  Failed ("CONSTRAINT_ERROR NOT " & "RAISED - B.1");
               exception
                  when Constraint_Error =>
                     null;
                  when others =>
                     Failed ("WRONG EXCEPTION " & "RAISED - B.1");
               end;

               begin  -- ASSIGNMENT TO OUT PARAMETER.
                  Rec3 := Rec_Oops;
                  Failed ("CONSTRAINT_ERROR NOT " & "RAISED - B.2");
               exception
                  when Constraint_Error =>
                     null;
                  when others =>
                     Failed ("WRONG EXCEPTION " & "RAISED - B.2");
               end;

            end E;
         end T;

      begin

         Rec91 := (9, 9, "123456789");
         Rec92 := Rec91;
         Rec93 := Rec91;

         Rec_Oops := (4, 4, "OOPS");

      end Pkg;

   begin  -- (B)

      Pkg.T.E (Rec91, Rec92, Rec93);

   end;   -- (B)

   --------------------------------------------------

   declare  -- (C)

      package Pkg is

         subtype Intrange is Integer range 0 .. 31;

         type Rectype (Constraint : Intrange := 15) is limited private;

         task T is
            entry E
              (Rec1 : in Rectype; Rec2 : in out Rectype; Rec3 : out Rectype);
         end T;

      private

         type Rectype (Constraint : Intrange := 15) is record
            Intfld : Intrange;
            Strfld : String (1 .. Constraint);
         end record;
      end Pkg;

      Rec91, Rec92, Rec93 : Pkg.Rectype (9);
      Rec_Oops            : Pkg.Rectype (4);

      package body Pkg is

         task body T is
         begin
            accept E
              (Rec1 : in Rectype; Rec2 : in out Rectype; Rec3 : out Rectype) do

               if (not Rec1'Constrained) or (Rec1.Constraint /= 9) then
                  Failed
                    ("CONSTRAINT ON LIMITED " & "PRIVATE TYPE IN PARAMETER " &
                     "NOT RECOGNIZED");
               end if;

               begin  -- ASSIGNMENT TO IN OUT PARAMETER.
                  Rec2 := Rec_Oops;
                  Failed ("CONSTRAINT_ERROR NOT " & "RAISED - C.1");
               exception
                  when Constraint_Error =>
                     null;
                  when others =>
                     Failed ("WRONG EXCEPTION " & "RAISED - C.1");
               end;

               begin  -- ASSIGNMENT TO OUT PARAMETER.
                  Rec3 := Rec_Oops;
                  Failed ("CONSTRAINT_ERROR NOT RAISED " & "- C.2");
               exception
                  when Constraint_Error =>
                     null;
                  when others =>
                     Failed ("WRONG EXCEPTION " & "RAISED - C.2");
               end;

            end E;
         end T;

      begin

         Rec91 := (9, 9, "123456789");
         Rec92 := Rec91;
         Rec93 := Rec91;

         Rec_Oops := (4, 4, "OOPS");

      end Pkg;

   begin  -- (C)

      Pkg.T.E (Rec91, Rec92, Rec93);

   end;   -- (C)

   --------------------------------------------------

   Result;

end C95087c;
