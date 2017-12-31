-- CC3016B.ADA

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
--  CHECK THAT AN INSTANCE OF A GENERIC PACKAGE MUST DECLARE A PACKAGE.
--  CHECK THAT THE DECLARATIVE ITEMS IN AN INSTANTIATION OF A GENERIC
--  PACKAGE SPECIFICATION ARE ELABORATED IN THE ORDER DECLARED.

-- HISTORY:
--         EDWARD V. BERARD, 8 AUGUST 1990

with Report;

procedure Cc3016b is

   When_Elaborated : Natural := 0;

   type Real is digits 6;
   Real_Value : Real := 3.141_59;

   True_Value : Boolean := True;

   Character_Value : Character := 'Z';

   type Month_Type is
     (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
   type Day_Type is range 1 .. 31;
   type Year_Type is range 1_904 .. 2_050;
   type Date is record
      Month : Month_Type;
      Day   : Day_Type;
      Year  : Year_Type;
   end record;

   type Date_Access is access Date;

   This_Month : Month_Type := Aug;
   This_Year  : Year_Type  := 1_990;

   Today : Date := (Month => Aug, Day => 8, Year => 1_990);

   First_Date : Date_Access :=
     new Date'(Day => 6, Month => Jun, Year => 1_967);

   type Due_Dates is array (Month_Type range Jan .. Dec) of Date;
   Report_Dates : Due_Dates :=
     ((Jan, 23, 1_990), (Feb, 23, 1_990), (Mar, 23, 1_990), (Apr, 23, 1_990),
      (May, 23, 1_990), (Jun, 22, 1_990), (Jul, 23, 1_990), (Aug, 23, 1_990),
      (Sep, 24, 1_990), (Oct, 23, 1_990), (Nov, 23, 1_990), (Dec, 20, 1_990));

   type List_Index is range 1 .. 16;
   type List is array (List_Index) of Natural;
   Order_List : List := (others => 0);

   generic

      type Return_Type is private;
      Return_Value : in out Return_Type;
      Position : in Natural;
      Offset : in Natural;
      When_Elab : in out Natural;
      type Index is range <>;
      type List is array (Index) of Natural;
      Order_List : in out List;

   function Name (Value : in Natural) return Return_Type;

   function Name (Value : in Natural) return Return_Type is

   begin -- NAME

      if (Value = Position) then
         When_Elab                     := Natural'Succ (When_Elab);
         Order_List (Index (Position)) := When_Elab;
         return Return_Value;
      elsif (Value = (Position + Offset)) then
         When_Elab                              := Natural'Succ (When_Elab);
         Order_List (Index (Position + Offset)) := When_Elab;
         return Return_Value;
      end if;

   end Name;

   generic

      type First_Type is private;
      with function First (Position : in Natural) return First_Type;
      First_Value : in Natural;
      type Second_Type is private;
      with function Second (Position : in Natural) return Second_Type;
      Second_Value : in Natural;
      type Third_Type is private;
      with function Third (Position : in Natural) return Third_Type;
      Third_Value : in Natural;
      type Fourth_Type is private;
      with function Fourth (Position : in Natural) return Fourth_Type;
      Fourth_Value : in Natural;
      type Fifth_Type is private;
      with function Fifth (Position : in Natural) return Fifth_Type;
      Fifth_Value : in Natural;
      type Sixth_Type is private;
      with function Sixth (Position : in Natural) return Sixth_Type;
      Sixth_Value : in Natural;
      type Seventh_Type is private;
      with function Seventh (Position : in Natural) return Seventh_Type;
      Seventh_Value : in Natural;
      type Eighth_Type is private;
      with function Eighth (Position : in Natural) return Eighth_Type;
      Eighth_Value : in Natural;
      type Ninth_Type is private;
      with function Ninth (Position : in Natural) return Ninth_Type;
      Ninth_Value : in Natural;
      type Tenth_Type is private;
      with function Tenth (Position : in Natural) return Tenth_Type;
      Tenth_Value : in Natural;
      type Eleventh_Type is private;
      with function Eleventh (Position : in Natural) return Eleventh_Type;
      Eleventh_Value : in Natural;
      type Twelfth_Type is private;
      with function Twelfth (Position : in Natural) return Twelfth_Type;
      Twelfth_Value : in Natural;
      type Thirteenth_Type is private;
      with function Thirteenth (Position : in Natural) return Thirteenth_Type;
      Thirteenth_Value : in Natural;
      type Fourteenth_Type is private;
      with function Fourteenth (Position : in Natural) return Fourteenth_Type;
      Fourteenth_Value : in Natural;
      type Fifteenth_Type is private;
      with function Fifteenth (Position : in Natural) return Fifteenth_Type;
      Fifteenth_Value : in Natural;
      type Sixteenth_Type is private;
      with function Sixteenth (Position : in Natural) return Sixteenth_Type;
      Sixteenth_Value : in Natural;

   package Order_Package is

      A : First_Type      := First (First_Value);
      B : Second_Type     := Second (Second_Value);
      C : Third_Type      := Third (Third_Value);
      D : Fourth_Type     := Fourth (Fourth_Value);
      E : Fifth_Type      := Fifth (Fifth_Value);
      F : Sixth_Type      := Sixth (Sixth_Value);
      G : Seventh_Type    := Seventh (Seventh_Value);
      H : Eighth_Type     := Eighth (Eighth_Value);
      I : Ninth_Type      := Ninth (Ninth_Value);
      J : Tenth_Type      := Tenth (Tenth_Value);
      K : Eleventh_Type   := Eleventh (Eleventh_Value);
      L : Twelfth_Type    := Twelfth (Twelfth_Value);
      M : Thirteenth_Type := Thirteenth (Thirteenth_Value);
      N : Fourteenth_Type := Fourteenth (Fourteenth_Value);
      O : Fifteenth_Type  := Fifteenth (Fifteenth_Value);
      P : Sixteenth_Type  := Sixteenth (Sixteenth_Value);

   end Order_Package;

   function Bool is new Name (Return_Type => Boolean,
      Return_Value => True_Value, Position => 1, Offset => 8,
      When_Elab => When_Elaborated, Index => List_Index, List => List,
      Order_List                          => Order_List);

   function Int is new Name (Return_Type => Year_Type,
      Return_Value => This_Year, Position => 2, Offset => 8,
      When_Elab => When_Elaborated, Index => List_Index, List => List,
      Order_List                         => Order_List);

   function Float is new Name (Return_Type => Real, Return_Value => Real_Value,
      Position => 3, Offset => 8, When_Elab => When_Elaborated,
      Index => List_Index, List => List, Order_List => Order_List);

   function Char is new Name (Return_Type => Character,
      Return_Value => Character_Value, Position => 4, Offset => 8,
      When_Elab => When_Elaborated, Index => List_Index, List => List,
      Order_List                          => Order_List);

   function Enum is new Name (Return_Type => Month_Type,
      Return_Value => This_Month, Position => 5, Offset => 8,
      When_Elab => When_Elaborated, Index => List_Index, List => List,
      Order_List                          => Order_List);

   function Arry is new Name (Return_Type => Due_Dates,
      Return_Value => Report_Dates, Position => 6, Offset => 8,
      When_Elab => When_Elaborated, Index => List_Index, List => List,
      Order_List                          => Order_List);

   function Rcrd is new Name (Return_Type => Date, Return_Value => Today,
      Position => 7, Offset => 8, When_Elab => When_Elaborated,
      Index => List_Index, List => List, Order_List => Order_List);

   function Acss is new Name (Return_Type => Date_Access,
      Return_Value => First_Date, Position => 8, Offset => 8,
      When_Elab => When_Elaborated, Index => List_Index, List => List,
      Order_List                          => Order_List);

   package Elaboration_Order is new Order_Package
     (First_Type       => Boolean,
      First            => Bool,
      First_Value      => 1,
      Third_Type       => Real,
      Third            => Float,
      Third_Value      => 3,
      Second_Type      => Year_Type,    -- ORDERING OF PARAMETERS
      Second           => Int,          -- IS DELIBERATE.
      Second_Value     => 2, Fourth_Type => Character, Fourth => Char,
      Fourth_Value     => 4, Fifth_Type => Month_Type, Fifth => Enum,
      Fifth_Value      => 5, Sixth_Type => Due_Dates, Sixth => Arry,
      Sixth_Value      => 6, Seventh_Type => Date, Seventh => Rcrd,
      Seventh_Value    => 7, Eighth_Type => Date_Access, Eighth => Acss,
      Eighth_Value     => 8, Ninth_Type => Boolean, Ninth => Bool,
      Ninth_Value      => 9, Tenth_Type => Year_Type, Tenth => Int,
      Tenth_Value      => 10, Eleventh_Type => Real, Eleventh => Float,
      Eleventh_Value   => 11, Twelfth_Type => Character, Twelfth => Char,
      Twelfth_Value => 12, Thirteenth_Type => Month_Type, Thirteenth => Enum,
      Thirteenth_Value => 13, Fourteenth_Type => Due_Dates, Fourteenth => Arry,
      Fourteenth_Value => 14, Fifteenth_Type => Date, Fifteenth => Rcrd,
      Fifteenth_Value  => 15, Sixteenth_Type => Date_Access, Sixteenth => Acss,
      Sixteenth_Value  => 16);

begin
   Report.Test
     ("CC3016B",
      "CHECK THAT AN INSTANCE OF A GENERIC " &
      "PACKAGE MUST DECLARE A PACKAGE. CHECK THAT THE " &
      "DECLARATIVE ITEMS IN AN INSTANTIATION OF A GENERIC " &
      "PACKAGE SPECIFICATION ARE ELABORATED IN THE ORDER " & "DECLARED.");

   if Order_List (1) /= Report.Ident_Int (1) then
      Report.Failed ("BOOLEAN 1 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (2) /= Report.Ident_Int (2) then
      Report.Failed ("INTEGER TYPE 1 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (3) /= Report.Ident_Int (3) then
      Report.Failed ("REAL 1 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (4) /= Report.Ident_Int (4) then
      Report.Failed ("CHARACTER 1 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (5) /= Report.Ident_Int (5) then
      Report.Failed ("ENUMERATION 1 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (6) /= Report.Ident_Int (6) then
      Report.Failed ("ARRAY 1 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (7) /= Report.Ident_Int (7) then
      Report.Failed ("RECORD 1 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (8) /= Report.Ident_Int (8) then
      Report.Failed ("ACCESS 1 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (9) /= Report.Ident_Int (9) then
      Report.Failed ("BOOLEAN 2 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (10) /= Report.Ident_Int (10) then
      Report.Failed ("INTEGER TYPE 2 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (11) /= Report.Ident_Int (11) then
      Report.Failed ("REAL 2 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (12) /= Report.Ident_Int (12) then
      Report.Failed ("CHARACTER 2 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (13) /= Report.Ident_Int (13) then
      Report.Failed ("ENUMERATION 2 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (14) /= Report.Ident_Int (14) then
      Report.Failed ("ARRAY 2 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (15) /= Report.Ident_Int (15) then
      Report.Failed ("RECORD 2 ELABORATED OUT OF ORDER");
   end if;

   if Order_List (16) /= Report.Ident_Int (16) then
      Report.Failed ("ACCESS 2 ELABORATED OUT OF ORDER");
   end if;

   Report.Result;

end Cc3016b;
