with Report;
with System.Rpc;
with Ada.Streams;
with Cxe5001_1;
procedure Cxe5001 is
   package Rpc renames System.Rpc;

begin

   Report.Test ("CXE5001", "Check the specification of System.RPC");

   declare  -- encapsulate the test

      Tc_Int : Integer := 1;

      Tc_Stream_Element_Array  : Ada.Streams.Stream_Element_Array (1 .. 100);
      Tc_Stream_Element_Offset : Ada.Streams.Stream_Element_Offset;

      --  Note: In the following, the RPC spec. being tested is shown
      --         as comment lines
      --
      --   type Partition_ID is range 0 .. 100;
      Tst_Partition_Id : Rpc.Partition_Id := Rpc.Partition_Id'First;

      --
      --    type Params_Stream_Type(
      --       Initial_Size : Ada.Streams.Stream_Element_Count) is new
      --       Ada.Streams.Root_Stream_Type with private;
      Tst_Params_Stream_Type : Rpc.Params_Stream_Type (10);

      type Acc_Params_Stream_Type is access Rpc.Params_Stream_Type;

      In_Params  : Acc_Params_Stream_Type := null;
      Out_Params : Acc_Params_Stream_Type;

      Tst_Rpc_Receiver_1 : Rpc.Rpc_Receiver := Cxe5001_1'Access;

      Some_Partition : Rpc.Partition_Id := Rpc.Partition_Id'Last;

   begin    -- encapsulation

      -- Arrange that the calls to the subprograms are compiled but not
      -- executed
      --
      if not Report.Equal (Tc_Int, Tc_Int) then

         --    procedure Read(
         --       Stream : in out Params_Stream_Type;
         --       Item : out Ada.Streams.Stream_Element_Array;
         --       Last : out Ada.Streams.Stream_Element_Offset);
         Rpc.Read
           (Tst_Params_Stream_Type, Tc_Stream_Element_Array,
            Tc_Stream_Element_Offset);

         --    procedure Write(
         --       Stream : in out Params_Stream_Type;
         --       Item : in Ada.Streams.Stream_Element_Array);
         Rpc.Write (Tst_Params_Stream_Type, Tc_Stream_Element_Array);

         --    -- Synchronous call
         --    procedure Do_RPC(
         --       Partition  : in Partition_ID;
         --       Params     : access Params_Stream_Type;
         --       Result     : access Params_Stream_Type);
         Rpc.Do_Rpc (Tst_Partition_Id, In_Params, Out_Params);

         --    -- Asynchronous call
         --    procedure Do_APC(
         --       Partition  : in Partition_ID;
         --       Params     : access Params_Stream_Type);
         Rpc.Do_Apc (Tst_Partition_Id, In_Params);

         --    procedure Establish_RPC_Receiver(
         --       Receiver : in RPC_Receiver);
         Rpc.Establish_Rpc_Receiver (Some_Partition, Tst_Rpc_Receiver_1);

         --    Communication_Error : exception;
         --    NOTE:  this check is out of LRM order to avoid the
         --    possibility  of compilers complaining about unreachable code
         raise Rpc.Communication_Error;

      end if;

   end;     -- encapsulation

   Report.Result;

end Cxe5001;
