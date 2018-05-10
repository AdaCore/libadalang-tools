--=================================================================--

package body Ca11018_0.Ca11018_1 is

   function Find_Word (Wrd : in Message; Msg : in Msg_Type) return Count is

      Num : Count := Count'First;

      -- Count how many time the word appears within the given message.

   begin
      -- ... Error-checking code omitted for brevity.

      for I in 1 .. (Msg.The_Length - Wrd'Length + 1) loop
         -- Parent's private type
         if Msg.The_Content (I .. I + Wrd'Length - 1) = Wrd
            -- Parent's private type
             then
            Num := Num + 1;
         end if;

      end loop;

      Tc_Function_Called := True;

      return (Num);

   end Find_Word;

end Ca11018_0.Ca11018_1;
