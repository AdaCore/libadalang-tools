with Report;       use Report;
with Cc3605a_Pack; use Cc3605a_Pack;

procedure Cc3605a is

   subtype Zero_To_Ten is Integer range Ident_Int (0) .. Ident_Int (10);

   subtype One_To_Five is Integer range Ident_Int (1) .. Ident_Int (5);

   Subprg_Act : Boolean := False;
begin
   Test
     ("CC3605A",
      "CHECK THAT SOME DIFFERENCES BETWEEN THE " &
      "FORMAL AND THE ACTUAL PARAMETERS DO NOT " &
      "INVALIDATE A MATCH");

----------------------------------------------------------------------
-- DIFFERENT PARAMETER NAMES
----------------------------------------------------------------------

   declare

      procedure Act_Proc (Diff_Name_Parm : One_To_Five) is
      begin
         Subprg_Act := True;
      end Act_Proc;

      generic

         with procedure Passed_Proc (Parm : One_To_Five);

      procedure Gen_Proc;

      procedure Gen_Proc is
      begin
         Passed_Proc (One_To_Five'First);
      end Gen_Proc;

      procedure Inst_Proc is new Gen_Proc (Act_Proc);
   begin
      Inst_Proc;
      if not Subprg_Act then
         Failed ("DIFFERENT PARAMETER NAMES MADE MATCH INVALID");
      end if;
   end;

----------------------------------------------------------------------
-- DIFFERENT PARAMETER CONSTRAINTS
----------------------------------------------------------------------

   declare

      procedure Act_Proc (Parm : One_To_Five) is
      begin
         Subprg_Act := True;
      end Act_Proc;

      generic

         with procedure Passed_Proc (Parm : Zero_To_Ten);

      procedure Gen_Proc;

      procedure Gen_Proc is
      begin
         Passed_Proc (One_To_Five'First);
      end Gen_Proc;

      procedure Inst_Proc is new Gen_Proc (Act_Proc);
   begin
      Subprg_Act := False;
      Inst_Proc;
      if not Subprg_Act then
         Failed ("DIFFERENT PARAMETER CONSTRAINTS MADE MATCH " & "INVALID");
      end if;
   end;

----------------------------------------------------------------------
-- ONE PARAMETER CONSTRAINED (ARRAY)
----------------------------------------------------------------------

   declare

      type Arr_Type is array (Integer range <>) of Boolean;

      subtype Arr_Const is Arr_Type (One_To_Five'First .. One_To_Five'Last);

      Passed_Parm : Arr_Const := (others => True);

      procedure Act_Proc (Parm : Arr_Const) is
      begin
         Subprg_Act := True;
      end Act_Proc;

      generic

         with procedure Passed_Proc (Parm : Arr_Type);

      procedure Gen_Proc;

      procedure Gen_Proc is
      begin
         Passed_Proc (Passed_Parm);
      end Gen_Proc;

      procedure Inst_Proc is new Gen_Proc (Act_Proc);
   begin
      Subprg_Act := False;
      Inst_Proc;
      if not Subprg_Act then
         Failed ("ONE ARRAY PARAMETER CONSTRAINED MADE MATCH " & "INVALID");
      end if;
   end;

----------------------------------------------------------------------
-- ONE PARAMETER CONSTRAINED (RECORDS)
----------------------------------------------------------------------

   declare

      type Rec_Type (Bol : Boolean) is record
         Sub_A : Integer;
         case Bol is
            when True =>
               Dscr_A : Integer;

            when False =>
               Dscr_B : Boolean;

         end case;
      end record;

      subtype Rec_Const is Rec_Type (True);

      Passed_Parm : Rec_Const := (True, 1, 2);

      procedure Act_Proc (Parm : Rec_Const) is
      begin
         Subprg_Act := True;
      end Act_Proc;

      generic

         with procedure Passed_Proc (Parm : Rec_Type);

      procedure Gen_Proc;

      procedure Gen_Proc is
      begin
         Passed_Proc (Passed_Parm);
      end Gen_Proc;

      procedure Inst_Proc is new Gen_Proc (Act_Proc);
   begin
      Subprg_Act := False;
      Inst_Proc;
      if not Subprg_Act then
         Failed ("ONE RECORD PARAMETER CONSTRAINED MADE MATCH " & "INVALID");
      end if;
   end;

----------------------------------------------------------------------
-- ONE PARAMETER CONSTRAINED (ACCESS)
----------------------------------------------------------------------

   declare

      type Arr_Type is array (Integer range <>) of Boolean;

      subtype Arr_Const is Arr_Type (One_To_Five'First .. One_To_Five'Last);

      type Arr_Acc_Type is access Arr_Type;

      subtype Arr_Acc_Const is Arr_Acc_Type (1 .. 3);

      Passed_Parm : Arr_Acc_Type := null;

      procedure Act_Proc (Parm : Arr_Acc_Const) is
      begin
         Subprg_Act := True;
      end Act_Proc;

      generic

         with procedure Passed_Proc (Parm : Arr_Acc_Type);

      procedure Gen_Proc;

      procedure Gen_Proc is
      begin
         Passed_Proc (Passed_Parm);
      end Gen_Proc;

      procedure Inst_Proc is new Gen_Proc (Act_Proc);
   begin
      Subprg_Act := False;
      Inst_Proc;
      if not Subprg_Act then
         Failed ("ONE ACCESS PARAMETER CONSTRAINED MADE MATCH " & "INVALID");
      end if;
   end;

----------------------------------------------------------------------
-- ONE PARAMETER CONSTRAINED (PRIVATE)
----------------------------------------------------------------------

   declare
      Passed_Parm : Pri_Const;

      procedure Act_Proc (Parm : Pri_Const) is
      begin
         Subprg_Act := True;
      end Act_Proc;

      generic

         with procedure Passed_Proc (Parm : Pri_Type);

      procedure Gen_Proc;

      procedure Gen_Proc is
      begin
         Passed_Proc (Passed_Parm);
      end Gen_Proc;

      procedure Inst_Proc is new Gen_Proc (Act_Proc);
   begin
      Subprg_Act := False;
      Inst_Proc;
      if not Subprg_Act then
         Failed ("ONE PRIVATE PARAMETER CONSTRAINED MADE MATCH " & "INVALID");
      end if;
   end;

----------------------------------------------------------------------
-- PRESENCE (OR ABSENCE) OF AN EXPLICIT "IN" MODE
----------------------------------------------------------------------

   declare

      procedure Act_Proc (Parm : Integer) is
      begin
         Subprg_Act := True;
      end Act_Proc;

      generic

         with procedure Passed_Proc (Parm : in Integer);

      procedure Gen_Proc;

      procedure Gen_Proc is
      begin
         Passed_Proc (1);
      end Gen_Proc;

      procedure Inst_Proc is new Gen_Proc (Act_Proc);
   begin
      Subprg_Act := False;
      Inst_Proc;
      if not Subprg_Act then
         Failed ("PRESENCE OF AN EXPLICIT 'IN' MODE MADE MATCH " & "INVALID");
      end if;
   end;

----------------------------------------------------------------------
-- DIFFERENT TYPE MARKS
----------------------------------------------------------------------

   declare

      subtype Mark_1_Type is Integer;

      subtype Mark_2_Type is Integer;

      procedure Act_Proc (Parm1 : in Mark_1_Type) is
      begin
         Subprg_Act := True;
      end Act_Proc;

      generic

         with procedure Passed_Proc (Parm2 : Mark_2_Type);

      procedure Gen_Proc;

      procedure Gen_Proc is
      begin
         Passed_Proc (1);
      end Gen_Proc;

      procedure Inst_Proc is new Gen_Proc (Act_Proc);
   begin
      Subprg_Act := False;
      Inst_Proc;
      if not Subprg_Act then
         Failed ("DIFFERENT TYPE MARKS MADE MATCH INVALID");
      end if;
   end;
   Result;
end Cc3605a;
