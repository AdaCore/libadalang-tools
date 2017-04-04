-- C95087D.ADA

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
--    CONSTRAINTS DO NOT RAISE CONSTRAINT_ERROR IF THE ACTUAL PARAMETER
--    IS UNCONSTRAINED, EVEN IF THE CONSTRAINT VALUES OF THE OBJECT
--    BEING ASSIGNED ARE DIFFERENT THAN THOSE OF THE ACTUAL PARAMETER.

--    SUBTESTS ARE:
--        (A) UNCONSTRAINED ACTUAL PARAMETERS OF RECORD TYPE.
--        (B) UNCONSTRAINED ACTUAL PARAMETERS OF PRIVATE TYPE.
--        (C) UNCONSTRAINED ACTUAL PARAMETERS OF LIMITED PRIVATE TYPE.

-- RJW  1/17/86

with Report; use Report;
procedure C95087d is

begin

   Test
     ("C95087D",
      "CHECK ASSIGNMENTS TO ENTRY FORMAL PARAMETERS " &
      "OF UNCONSTRAINED TYPES WITH UNCONSTRAINED " &
      "ACTUAL PARAMETERS");

   --------------------------------------------------

   declare  -- (A)

      package Pkg is

         subtype Intrange is Integer range 0 .. 31;

         type Rectype (Constraint : Intrange := 15) is record
            Intfld : Intrange;
            Strfld : String (1 .. Constraint);
         end record;

         task T is
            entry E
              (Rec1 : in     Rectype;
               Rec2 : in out Rectype;
               Rec3 :    out Rectype);
         end T;

      end Pkg;

      Rec91,
      Rec92,
      Rec93 : Pkg.Rectype :=
        (Ident_Int (5), 5, Ident_Str ("12345"));
      Rec_Oops : Pkg.Rectype;

      package body Pkg is

         task body T is
         begin
            accept E
              (Rec1 : in     Rectype;
               Rec2 : in out Rectype;
               Rec3 :    out Rectype) do

               if not Rec1'Constrained then
                  Failed ("REC1 IS NOT CONSTRAINED - A.1");
               end if;
               if Rec1.Constraint /= Ident_Int (9) then
                  Failed ("REC1 CONSTRAINT IS NOT 9 " & "- A.1");
               end if;

               begin  -- ASSIGNMENT TO IN OUT PARAMETER.
                  Rec2 := Rec_Oops;
               exception
                  when others =>
                     Failed ("EXCEPTION RAISED - A.1");
               end;

               begin  -- ASSIGNMENT TO OUT PARAMETER.
                  Rec3 := Rec_Oops;
               exception
                  when others =>
                     Failed ("EXCEPTION RAISED - A.2");
               end;

            end E;
         end T;

      begin

         Rec91 := (9, 9, "123456789");
         Rec92 := Rec91;
         Rec93 := Rec91;

         Rec_Oops := (4, 4, "OOPS");

      end Pkg;

      use Pkg;

   begin  -- (A)

      Pkg.T.E (Rec91, Rec92, Rec93);
      if (Rec92 /= Rec_Oops) or (Rec93 /= Rec_Oops) then
         Failed ("RESULTANT VALUE OF REC92 OR REC93 INCORRECT");
      end if;

   end;   -- (A)

   --------------------------------------------------

   declare  -- (B)

      package Pkg is

         subtype Intrange is Integer range 0 .. 31;

         type Rectype (Constraint : Intrange := 15) is private;

         task T is
            entry E
              (Rec1 : in     Rectype;
               Rec2 : in out Rectype;
               Rec3 :    out Rectype);
         end T;

      private

         type Rectype (Constraint : Intrange := 15) is record
            Intfld : Intrange;
            Strfld : String (1 .. Constraint);
         end record;
      end Pkg;

      Rec91, Rec92, Rec93 : Pkg.Rectype;
      Rec_Oops            : Pkg.Rectype;

      package body Pkg is

         task body T is
         begin
            accept E
              (Rec1 : in     Rectype;
               Rec2 : in out Rectype;
               Rec3 :    out Rectype) do

               if Rec3'Constrained then
                  Failed ("REC3 IS CONSTRAINED - B.1");
               end if;

               begin  -- ASSIGNMENT TO IN OUT PARAMETER.
                  Rec2 := Rec_Oops;
               exception
                  when others =>
                     Failed ("EXCEPTION RAISED - B.1");
               end;

               begin  -- ASSIGNMENT TO OUT PARAMETER.
                  Rec3 := Rec_Oops;
               exception
                  when others =>
                     Failed ("EXCEPTION RAISED - B.2");
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
              (Rec1 : in     Rectype;
               Rec2 : in out Rectype;
               Rec3 :    out Rectype);
         end T;

      private

         type Rectype (Constraint : Intrange := 15) is record
            Intfld : Intrange;
            Strfld : String (1 .. Constraint);
         end record;
      end Pkg;

      Rec91, Rec92, Rec93 : Pkg.Rectype;
      Rec_Oops            : Pkg.Rectype;

      package body Pkg is

         task body T is
         begin
            accept E
              (Rec1 : in     Rectype;
               Rec2 : in out Rectype;
               Rec3 :    out Rectype) do

               begin  -- ASSIGNMENT TO IN OUT PARAMETER.
                  Rec2 := Rec_Oops;
               exception
                  when others =>
                     Failed ("EXCEPTION RAISED - C.1");
               end;

               begin  -- ASSIGNMENT TO OUT PARAMETER.
                  Rec3 := Rec_Oops;
               exception
                  when others =>
                     Failed ("EXCEPTION RAISED - C.2");
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

end C95087d;
