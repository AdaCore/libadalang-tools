package Csc_Channel.Version_Handling is
   type T_Version_Handling is limited interface;
   type T_Version_Handling_Class_Access is access all T_Version_Handling'Class;

   procedure V_Receive_Version
      (This    :        not null access T_Version_Handling;
       Test    :        access T_Version_Handling;
       Version : in     Lang.Primitifs.T_Uint;
       P1      :    out Lang.Primitifs.T_Uint;
       P2      :    out Lang.Primitifs.T_Uint) is abstract;
end Csc_Channel.Version_Handling;
