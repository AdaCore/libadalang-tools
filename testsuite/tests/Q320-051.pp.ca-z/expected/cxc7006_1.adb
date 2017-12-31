with Report;
package body Cxc7006_1 is

   type Bool_Array is array (Natural range <>) of Boolean;
   type Test_Proc_Array is array (Natural range <>) of Handler_Test_Procedure;
   type Int_Array is array (Natural range <>) of Integer;

   type Task_Id_Array_Acc is access all Task_Id_Array;
   type Cause_Array_Acc is access all Cause_Array;
   type Handler_Id_Array_Acc is access all Handler_Id_Array;
   type Bool_Array_Acc is access all Bool_Array;
   type Test_Proc_Array_Acc is access all Test_Proc_Array;
   type Int_Array_Acc is access all Int_Array;
   type Exception_Array_Acc is access all Exception_Array;

   Test_Name           : String_Acc;
   Id_Array            : Task_Id_Array_Acc;
   Expected_Cause      : Cause_Array_Acc;
   Expected_Handler    : Handler_Id_Array_Acc;
   Handler_Called      : Bool_Array_Acc;
   Which_Handler       : Handler_Id_Array_Acc;
   Expected_Message    : String_Acc;
   Expected_Exception  : Ae.Exception_Id;
   Expected_Exceptions : Exception_Array_Acc;
   -- if null, then exceptions are checked against Expected_Exception
   Test_Procs       : Test_Proc_Array_Acc;
   Test_Proc_Params : Int_Array_Acc;

   procedure Start_Test (Name : String) is
   begin
      Report.Comment ("Starting test: " & Name);
      Test_Name        := new String'(Name);
      Id_Array         := null;
      Expected_Cause   := null;
      Handler_Called   := null;
      Expected_Handler := null;
      Expected_Message := null;
      Test_Procs       := null;
      Test_Proc_Params := null;
   end Start_Test;

   procedure Set_Task_Ids (Ids : Task_Id_Array) is
   begin
      Id_Array             := new Task_Id_Array'(Ids);
      Handler_Called       := new Bool_Array (Ids'Range);
      Handler_Called.all   := (others => False);
      Which_Handler        := new Handler_Id_Array (Ids'Range);
      Which_Handler.all    := (others => 0);
      Expected_Handler     := new Handler_Id_Array (Ids'Range);
      Expected_Handler.all := (others => 0);
      Test_Procs           := new Test_Proc_Array (Ids'Range);
      Test_Procs.all       := (others => null);
      Test_Proc_Params     := new Int_Array (Ids'Range);
   end Set_Task_Ids;

   procedure Set_Expected_Causes (Causes : Cause_Array) is
   begin
      Expected_Cause := new Cause_Array'(Causes);
   end Set_Expected_Causes;

   procedure Set_Expected_Handlers (Ids : Handler_Id_Array) is
   begin
      Expected_Handler.all := Ids;
   end Set_Expected_Handlers;

   procedure Set_Expected_Message (Message : String) is
   begin
      Expected_Message := new String'(Message);
   end Set_Expected_Message;

   procedure Set_Expected_Exception (Id : Ae.Exception_Id) is
   begin
      Expected_Exception  := Id;
      Expected_Exceptions := null;
   end Set_Expected_Exception;

   procedure Set_Expected_Exceptions (Ids : Exception_Array) is
   begin
      Expected_Exceptions := new Exception_Array'(Ids);
   end Set_Expected_Exceptions;

   procedure Set_Handler_Test (Index : in Integer;
      Test_Proc : in Handler_Test_Procedure; Param : in Integer)
   is
   begin
      if Index in Test_Procs'Range then
         Test_Procs (Index)       := Test_Proc;
         Test_Proc_Params (Index) := Param;
      end if;
   end Set_Handler_Test;

   protected Pt is
      procedure Handler_0
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence);
      procedure Handler_1
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence);
      procedure Handler_2
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence);
      procedure Handler_3
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence);
   end Pt;

   function The_Handler (Which : Handler_Id) return Att.Termination_Handler is
   begin
      case Which is
         when 0 =>
            return Pt.Handler_0'Access;
         when 1 =>
            return Pt.Handler_1'Access;
         when 2 =>
            return Pt.Handler_2'Access;
         when 3 =>
            return Pt.Handler_3'Access;
      end case;
   end The_Handler;

   protected body Pt is

      procedure Handler (Id : in Handler_Id;
         Cause              : in Ada.Task_Termination.Cause_Of_Termination;
         T                  : in Ada.Task_Identification.Task_Id;
         X                  : in Ada.Exceptions.Exception_Occurrence)
      is
         use type Att.Cause_Of_Termination;
         use type Ati.Task_Id;
         use type Ae.Exception_Occurrence;
         use type Ae.Exception_Id;
         Ok      : Boolean;
         Err_Msg : String_Acc;
      begin
         for I in Id_Array'Range loop
            if T = Id_Array (I) then
               if Handler_Called (I) then
                  Report.Failed
                    (Test_Name.all & ": Handler called twice for same task");
               end if;
               Handler_Called (I) := True;
               Which_Handler (I)  := Id;
               if Cause /= Expected_Cause (I) then
                  Report.Failed
                    (Test_Name.all & ": Handler called with wrong Cause");
               end if;
               if Cause = Att.Unhandled_Exception then
                  if Ae.Exception_Identity (X) = Ae.Null_Id then
                     Report.Failed
                       (Test_Name.all &
                        ": Unhandled_Exception handler called with " &
                        "null occurrence");
                  elsif
                    (Expected_Exceptions = null
                     and then Ae.Exception_Identity (X) /= Expected_Exception)
                    or else
                    (Expected_Exceptions /= null
                     and then Ae.Exception_Identity (X) /=
                       Expected_Exceptions (I))
                  then
                     Report.Failed
                       (Test_Name.all &
                        ": Unhandled_Exception occurrence has wrong exception");
                  elsif Ae.Exception_Identity (X) /= Program_Error'Identity
                    and then Ae.Exception_Message (X) /= Expected_Message.all
                  then
                     Report.Failed
                       (Test_Name.all &
                        ": Unhandled_Exception occurrence has wrong message");
                  end if;
               else
                  if Ae.Exception_Identity (X) /= Ae.Null_Id then
                     Report.Failed
                       (Test_Name.all &
                        ": Normal or Abnormal handler called with " &
                        "non-null occurrence");
                  end if;
               end if;
               if Test_Procs (I) /= null then
                  Test_Procs (I).all (Test_Proc_Params (I), Ok, Err_Msg);
                  if not Ok then
                     Report.Failed (Test_Name.all & ": " & Err_Msg.all);
                  end if;
               end if;
            end if;
         end loop;
      end Handler;

      procedure Handler_0
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence)
      is
      begin
         Handler (0, Cause, T, X);
      end Handler_0;

      procedure Handler_1
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence)
      is
      begin
         Handler (1, Cause, T, X);
      end Handler_1;

      procedure Handler_2
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence)
      is
      begin
         Handler (2, Cause, T, X);
      end Handler_2;

      procedure Handler_3
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence)
      is
      begin
         Handler (3, Cause, T, X);
      end Handler_3;

   end Pt;

   procedure Test_All_Handlers_Called is
      Not_Called : Boolean := False;
   begin
      for I in Handler_Called'Range loop
         if not Handler_Called (I) then
            if not Not_Called then
               Report.Failed
                 (Test_Name.all &
                  ": Termination handler not called for all tasks");
               Not_Called := True;
            end if;
         elsif Which_Handler (I) /= Expected_Handler (I) then
            Report.Failed
              (Test_Name.all & ": Wrong handler called: expected " &
               Integer'Image (Expected_Handler (I)) & ", got " &
               Integer'Image (Which_Handler (I)));
         end if;
      end loop;
   end Test_All_Handlers_Called;

end Cxc7006_1;
