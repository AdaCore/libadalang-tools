

with Report;
with System.RPC;
with Ada.Streams;
with CXE5001_1;
procedure CXE5001 is
   package RPC renames System.RPC;

begin

   Report.Test ("CXE5001", "Check the specification of System.RPC");


   declare  -- encapsulate the test

      TC_Int    : integer := 1;

      TC_Stream_Element_Array :
              Ada.Streams.Stream_Element_Array(1..100);
      TC_Stream_Element_Offset : Ada.Streams.Stream_Element_Offset;

      --  Note:  In the following, the RPC spec. being tested is shown
      --         as comment lines
      --
      --   type Partition_ID is range 0 .. 100;
      TST_Partition_ID : RPC.Partition_ID := RPC.Partition_ID'First;

      --
      --    type Params_Stream_Type(
      --       Initial_Size : Ada.Streams.Stream_Element_Count) is new
      --       Ada.Streams.Root_Stream_Type with private;
      TST_Params_Stream_Type :
                          RPC.Params_Stream_Type (10);

      type acc_Params_Stream_Type is  access RPC.Params_Stream_Type;

      In_Params  :  acc_Params_Stream_Type := null;
      Out_Params :  acc_Params_Stream_Type;

      TST_RPC_Receiver_1 : RPC.RPC_Receiver := CXE5001_1'Access;

      Some_Partition : RPC.Partition_ID := RPC.Partition_ID'Last;


   begin    -- encapsulation

      -- Arrange that the calls to the subprograms are compiled but
      -- not executed
      --
      if not Report.Equal ( TC_Int, TC_Int ) then

         --    procedure Read(
         --       Stream : in out Params_Stream_Type;
         --       Item : out Ada.Streams.Stream_Element_Array;
         --       Last : out Ada.Streams.Stream_Element_Offset);
         RPC.Read ( TST_Params_Stream_Type,
                    TC_Stream_Element_Array,
                    TC_Stream_Element_Offset );

         --    procedure Write(
         --       Stream : in out Params_Stream_Type;
         --       Item : in Ada.Streams.Stream_Element_Array);
         RPC.Write ( TST_Params_Stream_Type, TC_Stream_Element_Array );

         --    -- Synchronous call
         --    procedure Do_RPC(
         --       Partition  : in Partition_ID;
         --       Params     : access Params_Stream_Type;
         --       Result     : access Params_Stream_Type);
         RPC.Do_RPC ( TST_Partition_ID, In_Params, Out_Params );


         --    -- Asynchronous call
         --    procedure Do_APC(
         --       Partition  : in Partition_ID;
         --       Params     : access Params_Stream_Type);
         RPC.Do_APC ( TST_Partition_ID, In_Params );


         --    procedure Establish_RPC_Receiver(
         --       Receiver : in RPC_Receiver);
         RPC.Establish_RPC_Receiver (  Some_Partition, TST_RPC_Receiver_1 );



         --    Communication_Error : exception;
         --    NOTE:  this check is out of LRM order to avoid the
         --    possibility  of compilers complaining about unreachable code
         raise RPC.Communication_Error;

      end if;

   end;     -- encapsulation

   Report.Result;

end CXE5001;
