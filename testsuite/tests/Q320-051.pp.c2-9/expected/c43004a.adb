-- C43004A.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED IF A VALUE FOR A
--     NON-DISCRIMINANT SCALAR COMPONENT OF AN AGGREGATE IS NOT
--     WITHIN THE RANGE OF THE COMPONENT'S SUBTYPE.

-- HISTORY:
--     BCB 01/22/88  CREATED ORIGINAL TEST.
--     RJW 06/27/90  CORRECTED CONSTRAINTS OF TYPE DFIX.
--     LDC 09/25/90  ADDED A BLOCK IN THE EXCEPTION HANDLER SO IT CAN
--                   NOT OPTIMIZE IT AWAY,  ALSO INITIALIZED EACH
--                   OBJECT TO VALID DATA BEFORE DOING THE INVALID,
--                   MADE 'IDENT_XXX' FUNCTIONS SO THE COMPILER CAN
--                   NOT JUST EVALUATE THE ASSIGNMENT AND PUT IN CODE
--                   FOR A CONSTRAINT ERROR IN IS PLACE.
--     JRL 06/07/96  Changed value in aggregate in subtest 4 to value
--                   guaranteed to be in the base range of the type FIX.
--                   Corrected typo.

with Report; use Report;

procedure C43004a is

   type Int is range 1 .. 8;
   subtype Sint is Int range 2 .. 7;

   type Enum is (Vince, John, Tom, Phil, Rosa, Jodie, Brian, Dave);
   subtype Senum is Enum range John .. Brian;

   type Fl is digits 5 range 0.0 .. 10.0;
   subtype Sfl is Fl range 1.0 .. 9.0;

   type Fix is delta 0.25 range 0.0 .. 8.0;
   subtype Sfix is Fix range 1.0 .. 7.0;

   type Dint is new Integer range 1 .. 8;
   subtype Sdint is Dint range 2 .. 7;

   type Denum is new Enum range Vince .. Dave;
   subtype Sdenum is Denum range John .. Brian;

   type Dfl is new Float range 0.0 .. 10.0;
   subtype Sdfl is Dfl range 1.0 .. 9.0;

   type Dfix is new Fix range 0.5 .. 7.5;
   subtype Sdfix is Dfix range 1.0 .. 7.0;

   type Rec1 is record
      E1, E2, E3, E4, E5 : Senum;
   end record;

   type Rec2 is record
      E1, E2, E3, E4, E5 : Sfix;
   end record;

   type Rec3 is record
      E1, E2, E3, E4, E5 : Sdenum;
   end record;

   type Rec4 is record
      E1, E2, E3, E4, E5 : Sdfix;
   end record;

   Array_Obj : array (1 .. 2) of Integer;

   A : array (1 .. 5) of Sint;
   B : Rec1;
   C : array (1 .. 5) of Sfl;
   D : Rec2;
   E : array (1 .. 5) of Sdint;
   F : Rec3;
   G : array (1 .. 5) of Sdfl;
   H : Rec4;

   generic
      type General_Purpose is private;
   function Genequal (One, Two : General_Purpose) return Boolean;

   function Genequal (One, Two : General_Purpose) return Boolean is
   begin
      if Equal (3, 3) then
         return One = Two;
      else
         return One /= Two;
      end if;
   end Genequal;

   function Equal is new Genequal (Senum);
   function Equal is new Genequal (Sfl);
   function Equal is new Genequal (Sfix);
   function Equal is new Genequal (Sdenum);
   function Equal is new Genequal (Sdfl);
   function Equal is new Genequal (Sdfix);

   generic
      type General_Purpose is private;
      with function Equal_General (One, Two : General_Purpose) return Boolean;
   function Gen_Ident (X : General_Purpose) return General_Purpose;
   function Gen_Ident (X : General_Purpose) return General_Purpose is
   begin
      if Equal_General (X, X) then  -- ALWAYS EQUAL.
         return X;                -- ALWAYS EXECUTED.
      end if;
      -- NEVER EXECUTED.
      return X;
   end Gen_Ident;

   function Ident_Fl is new Gen_Ident (Fl, Equal);
   function Ident_Fix is new Gen_Ident (Fix, Equal);
   function Ident_Dfl is new Gen_Ident (Dfl, Equal);
   function Ident_Dfix is new Gen_Ident (Dfix, Equal);

begin
   Test
     ("C43004A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED IF A " &
      "VALUE FOR A NON-DISCRIMINANT SCALAR COMPONENT " &
      "OF AN AGGREGATE IS NOT WITHIN THE RANGE OF " &
      "THE COMPONENT'S SUBTYPE");

   Array_Obj := (1, 2);

   begin
      A := (2, 3, 4, 5, 6);            -- OK

      if Equal (Integer (A (Ident_Int (1))), Integer (A (Ident_Int (2)))) then
         Comment ("DON'T OPTIMIZE A");
      end if;

      A := (Sint (Ident_Int (1)), 2, 3, 4, 7);
      -- CONSTRAINT_ERROR BY AGGREGATE WITH INTEGER COMPONENTS.
      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 1");
      if Equal (Integer (A (Ident_Int (1))), Integer (A (Ident_Int (1)))) then
         Comment ("DON'T OPTIMIZE A");
      end if;
   exception
      when Constraint_Error =>
         if Equal (Array_Obj (Ident_Int (1)), Array_Obj (Ident_Int (2))) then
            Comment ("DON'T OPTIMIZE EXCEPTION HANDLER");
         end if;
      when others =>
         Failed
           ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " & "WAS RAISED - 1");
   end;

   begin
      B := (John, Tom, Phil, Rosa, John);  -- OK

      if Equal (B.E1, B.E2) then
         Comment ("DON'T OPTIMIZE B");
      end if;

      B := (Enum'Val (Ident_Int (Enum'Pos (Dave))), Tom, Phil, Rosa, Jodie);
      -- CONSTRAINT_ERROR BY AGGREGATE WITH COMPONENTS OF AN ENUMERATION TYPE.
      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 2");
      if not Equal (B.E1, B.E1) then
         Comment ("DON'T OPTIMIZE B");
      end if;
   exception
      when Constraint_Error =>
         if Equal (Array_Obj (Ident_Int (1)), Array_Obj (Ident_Int (2))) then
            Comment ("DON'T OPTIMIZE EXCEPTION HANDLER");
         end if;
      when others =>
         Failed
           ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " & "WAS RAISED - 2");
   end;
   begin
      C := (2.0, 3.0, 4.0, 5.0, 6.0);  -- OK
      if Equal (C (Ident_Int (1)), C (Ident_Int (2))) then
         Comment ("DON'T OPTIMIZE C");
      end if;

      C := (Ident_Fl (1.0), 2.0, 3.0, 4.0, Ident_Fl (10.0));
      -- CONSTRAINT_ERROR BY AGGREGATE WITH FLOATING POINT COMPONENTS.
      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 3");
      if not Equal (C (Ident_Int (1)), C (Ident_Int (1))) then
         Comment ("DON'T OPTIMIZE C");
      end if;
   exception
      when Constraint_Error =>
         if Equal (Array_Obj (Ident_Int (1)), Array_Obj (Ident_Int (2))) then
            Comment ("DON'T OPTIMIZE EXCEPTION HANDLER");
         end if;
      when others =>
         Failed
           ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " & "WAS RAISED - 3");
   end;

   begin
      D := (2.2, 3.3, 4.4, 5.5, 6.6); -- OK
      if Equal (D.E1, D.E5) then
         Comment ("DON'T OPTIMIZE D");
      end if;

      D := (Ident_Fix (1.0), 2.1, 3.3, 4.4, Ident_Fix (7.75));
      -- CONSTRAINT_ERROR BY AGGREGATE WITH FIXED POINT COMPONENTS.
      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 4");
      if not Equal (D.E5, D.E5) then
         Comment ("DON'T OPTIMIZE D");
      end if;
   exception
      when Constraint_Error =>
         if Equal (Array_Obj (Ident_Int (1)), Array_Obj (Ident_Int (2))) then
            Comment ("DON'T OPTIMIZE EXCEPTION HANDLER");
         end if;
      when others =>
         Failed
           ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " & "WAS RAISED - 4");
   end;

   begin
      E := (2, 3, 4, 5, 6); -- OK
      if Equal (Integer (E (Ident_Int (1))), Integer (E (Ident_Int (2)))) then
         Comment ("DON'T OPTIMIZE E");
      end if;

      E := (Sdint (Ident_Int (1)), 2, 3, 4, 7);
      -- CONSTRAINT_ERROR BY AGGREGATE WITH DERIVED INTEGER COMPONENTS.
      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 5");
      if not Equal
          (Integer (E (Ident_Int (1))),
           Integer (E (Ident_Int (1))))
      then
         Comment ("DON'T OPTIMIZE E");
      end if;
   exception
      when Constraint_Error =>
         if Equal (Array_Obj (Ident_Int (1)), Array_Obj (Ident_Int (2))) then
            Comment ("DON'T OPTIMIZE EXCEPTION HANDLER");
         end if;
      when others =>
         Failed
           ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " & "WAS RAISED - 5");
   end;

   begin
      F := (John, Tom, Phil, Rosa, John);  -- OK
      if Equal (F.E1, F.E2) then
         Comment ("DON'T OPTIMIZE F");
      end if;

      F := (Denum'Val (Ident_Int (Denum'Pos (Vince))), Tom, Phil, Rosa, Jodie);
      -- CONSTRAINT_ERROR BY AGGREGATE WITH COMPONENTS OF A DERIVED ENUMERATION
      -- TYPE.
      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 6");
      if not Equal (F.E1, F.E1) then
         Comment ("DON'T OPTIMIZE F");
      end if;
   exception
      when Constraint_Error =>
         if Equal (Array_Obj (Ident_Int (1)), Array_Obj (Ident_Int (2))) then
            Comment ("DON'T OPTIMIZE EXCEPTION HANDLER");
         end if;
      when others =>
         Failed
           ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " & "WAS RAISED - 6");
   end;

   begin
      G := (2.0, 3.0, 4.0, 5.0, 6.0); -- OK
      if Equal (G (Ident_Int (1)), G (Ident_Int (2))) then
         Comment ("DON'T OPTIMIZE G");
      end if;

      G := (Ident_Dfl (1.0), 2.0, 3.0, 4.0, Ident_Dfl (10.0));
      -- CONSTRAINT_ERROR BY AGGREGATE WITH DERIVED FLOATING POINT COMPONENTS.
      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 7");
      if not Equal (G (Ident_Int (1)), G (Ident_Int (1))) then
         Comment ("DON'T OPTIMIZE G");
      end if;
   exception
      when Constraint_Error =>
         if Equal (Array_Obj (Ident_Int (1)), Array_Obj (Ident_Int (2))) then
            Comment ("DON'T OPTIMIZE EXCEPTION HANDLER");
         end if;
      when others =>
         Failed
           ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " & "WAS RAISED - 7");
   end;

   begin
      H := (2.2, 3.3, 4.4, 5.5, 6.6); -- OK
      if Equal (H.E1, H.E2) then
         Comment ("DON'T OPTIMIZE H");
      end if;

      H := (Ident_Dfix (2.0), 2.5, 3.5, 4.3, Ident_Dfix (7.4));
      -- CONSTRAINT_ERROR BY AGGREGATE WITH DERIVED FIXED POINT COMPONENTS.
      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 8");
      if Equal (H.E1, H.E5) then
         Comment ("DON'T OPTIMIZE H");
      end if;
   exception
      when Constraint_Error =>
         if Equal (Array_Obj (Ident_Int (1)), Array_Obj (Ident_Int (2))) then
            Comment ("DON'T OPTIMIZE EXCEPTION HANDLER");
         end if;
      when others =>
         Failed
           ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " & "WAS RAISED - 8");
   end;

   Result;
end C43004a;
