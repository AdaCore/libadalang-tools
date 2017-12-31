-- C95087A.ADA

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
-- CHECK THAT UNCONSTRAINED RECORD, PRIVATE, LIMITED PRIVATE, AND ARRAY
--   FORMAL PARAMETERS USE THE CONSTRAINTS OF ACTUAL PARAMETERS.
--   SUBTESTS ARE:
--        (A) RECORD TYPE, UNCONSTRAINED ACTUALS, DEFAULTS.
--        (B) PRIVATE TYPE, CONSTRAINED ACTUALS, NO DEFAULTS.
--        (C) LIMITED PRIVATE TYPE, UNCONSTRAINED ACTUALS, NO DEFAULTS.
--        (D) ARRAY TYPE, CONSTRAINED ACTUALS, DEFAULTS.

-- GLH  7/19/85
-- JRK 8/23/85

with Report; use Report;
procedure C95087a is

begin
   Test
     ("C95087A",
      "CHECK USE OF ACTUAL CONSTRAINTS BY " &
      "UNCONSTRAINED FORMAL PARAMETERS");

   declare  -- (A)

      package Pkg is

         subtype Int is Integer range 0 .. 100;

         type Rectype (Constraint : Int := 80) is record
            Intfield : Integer;
            Strfield : String (1 .. Constraint);
         end record;

         Rec1 : Rectype := (10, 10, "0123456789");
         Rec2 : Rectype := (17, 7, "C95087A..........");
         Rec3 : Rectype := (1, 1, "A");
         Rec4 : Rectype;  -- 80.

         task T1 is
            entry E1 (Rec1 : in Rectype := (2, 0, "AB"); Rec2 : out Rectype;
               Rec3        : in out Rectype);
         end T1;

         task T2 is
            entry E2 (Rec : out Rectype);
         end T2;
      end Pkg;

      package body Pkg is

         task body T1 is
         begin
            accept E1 (Rec1 : in Rectype := (2, 0, "AB"); Rec2 : out Rectype;
               Rec3         : in out Rectype) do

               if Rec1.Constraint /= Ident_Int (10) then
                  Failed
                    ("RECORD TYPE IN PARAMETER " & "DID NOT USE CONSTRAINT " &
                     "OF ACTUAL");
               end if;
               if Rec2.Constraint /= Ident_Int (17) then
                  Failed
                    ("RECORD TYPE OUT " & "PARAMETER DID NOT USE " &
                     "CONSTRAINT OF ACTUAL");
               end if;
               if Rec3.Constraint /= Ident_Int (1) then
                  Failed
                    ("RECORD TYPE IN OUT " & "PARAMETER DID NOT USE " &
                     "CONSTRAINT OF ACTUAL");
               end if;
               Rec2 := Pkg.Rec2;
            end E1;
         end T1;

         task body T2 is
         begin
            accept E2 (Rec : out Rectype) do
               if Rec.Constraint /= Ident_Int (80) then
                  Failed
                    ("RECORD TYPE OUT " & "PARAMETER DID " &
                     "NOT USE CONSTRAINT OF " & "UNINITIALIZED ACTUAL");
               end if;
               Rec := (10, 10, "9876543210");
            end E2;
         end T2;
      end Pkg;

   begin  -- (A)

      Pkg.T1.E1 (Pkg.Rec1, Pkg.Rec2, Pkg.Rec3);
      Pkg.T2.E2 (Pkg.Rec4);

   end;   -- (A)

   ---------------------------------------------

   B :
   declare  -- (B)

      package Pkg is

         subtype Int is Integer range 0 .. 100;

         type Rectype (Constraint : Int := 80) is private;

         task T1 is
            entry E1 (Rec1 : in     Rectype; Rec2 : out Rectype;
               Rec3        : in out Rectype);
         end T1;

         task T2 is
            entry E2 (Rec : out Rectype);
         end T2;

      private
         type Rectype (Constraint : Int := 80) is record
            Intfield : Integer;
            Strfield : String (1 .. Constraint);
         end record;
      end Pkg;

      Rec1 : Pkg.Rectype (10);
      Rec2 : Pkg.Rectype (17);
      Rec3 : Pkg.Rectype (1);
      Rec4 : Pkg.Rectype (10);

      package body Pkg is

         task body T1 is
         begin
            accept E1 (Rec1 : in     Rectype; Rec2 : out Rectype;
               Rec3         : in out Rectype) do
               if Rec1.Constraint /= Ident_Int (10) then
                  Failed
                    ("PRIVATE TYPE IN " & "PARAMETER DID " &
                     "NOT USE CONSTRAINT OF " & "ACTUAL");
               end if;
               if Rec2.Constraint /= Ident_Int (17) then
                  Failed
                    ("PRIVATE TYPE OUT " & "PARAMETER DID " &
                     "NOT USE CONSTRAINT OF " & "ACTUAL");
               end if;
               if Rec3.Constraint /= Ident_Int (1) then
                  Failed
                    ("PRIVATE TYPE IN OUT " & "PARAMETER DID " &
                     "NOT USE CONSTRAINT OF " & "ACTUAL");
               end if;
               Rec2 := B.Rec2;
            end E1;
         end T1;

         task body T2 is
         begin
            accept E2 (Rec : out Rectype) do
               if Rec.Constraint /= Ident_Int (10) then
                  Failed
                    ("PRIVATE TYPE OUT " & "PARAMETER DID " &
                     "NOT USE CONSTRAINT OF " & "UNINITIALIZED ACTUAL");
               end if;
               Rec := (10, 10, "9876543210");
            end E2;
         end T2;

      begin
         Rec1 := (10, 10, "0123456789");
         Rec2 := (17, 7, "C95087A..........");
         Rec3 := (1, 1, "A");
      end Pkg;

   begin  -- (B)

      Pkg.T1.E1 (Rec1, Rec2, Rec3);
      Pkg.T2.E2 (Rec4);

   end B;  -- (B)

   ---------------------------------------------

   C :
   declare  -- (C)

      package Pkg is

         subtype Int is Integer range 0 .. 100;

         type Rectype (Constraint : Int := 80) is limited private;

         task T1 is
            entry E1 (Rec1 : in     Rectype; Rec2 : out Rectype;
               Rec3        : in out Rectype);
         end T1;

         task T2 is
            entry E2 (Rec : out Rectype);
         end T2;

      private
         type Rectype (Constraint : Int := 80) is record
            Intfield : Integer;
            Strfield : String (1 .. Constraint);
         end record;
      end Pkg;

      Rec1 : Pkg.Rectype;     -- 10.
      Rec2 : Pkg.Rectype;     -- 17.
      Rec3 : Pkg.Rectype;     --  1.
      Rec4 : Pkg.Rectype;     -- 80.

      package body Pkg is

         task body T1 is
         begin
            accept E1 (Rec1 : in     Rectype; Rec2 : out Rectype;
               Rec3         : in out Rectype) do
               if Rec1.Constraint /= Ident_Int (10) then
                  Failed
                    ("LIMITED PRIVATE TYPE IN " & "PARAMETER DID NOT USE " &
                     "CONSTRAINT OF ACTUAL");
               end if;
               if Rec2.Constraint /= Ident_Int (17) then
                  Failed
                    ("LIMITED PRIVATE TYPE OUT " & "PARAMETER DID NOT USE " &
                     "CONSTRAINT OF " & "ACTUAL");
               end if;
               if Rec3.Constraint /= Ident_Int (1) then
                  Failed
                    ("LIMITED PRIVATE TYPE IN " & "OUT PARAMETER DID NOT " &
                     "USE CONSTRAINT OF ACTUAL");
               end if;
               Rec2 := C.Rec2;
            end E1;
         end T1;

         task body T2 is
         begin
            accept E2 (Rec : out Rectype) do
               if Rec.Constraint /= Ident_Int (80) then
                  Failed
                    ("LIMITED PRIVATE TYPE OUT " & "PARAMETER DID NOT USE " &
                     "CONSTRAINT OF UNINITIALIZED " & "ACTUAL");
               end if;
               Rec := (10, 10, "9876543210");
            end E2;
         end T2;

      begin
         Rec1 := (10, 10, "0123456789");
         Rec2 := (17, 7, "C95087A..........");
         Rec3 := (1, 1, "A");
      end Pkg;

   begin  -- (C)

      Pkg.T1.E1 (Rec1, Rec2, Rec3);
      Pkg.T2.E2 (Rec4);

   end C;   -- (C)

   ---------------------------------------------

   D :
   declare  -- (D)

      type Atype is array (Integer range <>, Positive range <>) of Character;

      A1, A2, A3 : Atype (-1 .. 1, 4 .. 5) :=
        (('A', 'B'), ('C', 'D'), ('E', 'F'));

      A4 : Atype (-1 .. 1, 4 .. 5);

      Ca1 : constant Atype (8 .. 9, -7 .. Integer'First) :=
        (8 .. 9 => (-7 .. Integer'First => 'A'));

      S1 : String (1 .. Integer'First) := "";
      S2 : String (-5 .. -7)           := "";
      S3 : String (1 .. 0)             := "";

      task T1 is
         entry E1 (A1 : in Atype := Ca1; A2 : out Atype; A3 : in out Atype);
      end T1;

      task T2 is
         entry E2 (A4 : out Atype);
      end T2;

      task T3 is
         entry E3 (S1 : in String; S2 : in out String; S3 : out String);
      end T3;

      task body T1 is
      begin
         accept E1 (A1 : in Atype := Ca1; A2 : out Atype; A3 : in out Atype) do
            if A1'First (1) /= Ident_Int (-1) or
              A1'Last (1) /= Ident_Int (1) or A1'First (2) /= Ident_Int (4) or
              A1'Last (2) /= Ident_Int (5) then
               Failed
                 ("ARRAY TYPE IN PARAMETER DID " &
                  "NOT USE CONSTRAINTS OF ACTUAL");
            end if;
            if A2'First (1) /= Ident_Int (-1) or
              A2'Last (1) /= Ident_Int (1) or A2'First (2) /= Ident_Int (4) or
              A2'Last (2) /= Ident_Int (5) then
               Failed
                 ("ARRAY TYPE OUT PARAMETER DID " &
                  "NOT USE CONSTRAINTS OF ACTUAL");
            end if;
            if A3'First (1) /= Ident_Int (-1) or
              A3'Last (1) /= Ident_Int (1) or A3'First (2) /= Ident_Int (4) or
              A3'Last (2) /= Ident_Int (5) then
               Failed
                 ("ARRAY TYPE IN OUT PARAMETER " &
                  "DID NOT USE CONSTRAINTS OF " & "ACTUAL");
            end if;
            A2 := D.A2;
         end E1;
      end T1;

      task body T2 is
      begin
         accept E2 (A4 : out Atype) do
            if A4'First (1) /= Ident_Int (-1) or
              A4'Last (1) /= Ident_Int (1) or A4'First (2) /= Ident_Int (4) or
              A4'Last (2) /= Ident_Int (5) then
               Failed
                 ("ARRAY TYPE OUT PARAMETER DID " & "NOT USE CONSTRAINTS OF " &
                  "UNINITIALIZED ACTUAL");
            end if;
            A4 := A2;
         end E2;
      end T2;

      task body T3 is
      begin
         accept E3 (S1 : in String; S2 : in out String; S3 : out String) do
            if S1'First /= Ident_Int (1) or
              S1'Last /= Ident_Int (Integer'First) then
               Failed
                 ("STRING TYPE IN PARAMETER DID " &
                  "NOT USE CONSTRAINTS OF ACTUAL " & "NULL STRING");
            end if;
            if S2'First /= Ident_Int (-5) or S2'Last /= Ident_Int (-7) then
               Failed
                 ("STRING TYPE IN OUT PARAMETER " &
                  "DID NOT USE CONSTRAINTS OF " & "ACTUAL NULL STRING");
            end if;
            if S3'First /= Ident_Int (1) or S3'Last /= Ident_Int (0) then
               Failed
                 ("STRING TYPE OUT PARAMETER DID NOT " &
                  "USE CONSTRAINTS OF ACTUAL NULL " & "STRING");
            end if;
            S3 := "";
         end E3;
      end T3;

   begin  -- (D)

      T1.E1 (A1, A2, A3);
      T2.E2 (A4);
      T3.E3 (S1, S2, S3);

   end D;  -- (D)

   Result;
end C95087a;
