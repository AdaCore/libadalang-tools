     --==================================================================--

package body Cc51002_2 is

   -- The implementation of Send is purely artificial; the validity of its
   -- implementation in the context of the abstraction is irrelevant to the
   -- feature being tested.

   function Send (M : Who_Msg_Type; R : Cc51002_0.Recipients) return Boolean is
      use type Cc51002_0.Recipients;
   begin
      return
        (M.Text = "Willkommen" and M.From = Outside and R = Cc51002_0.Local);
   end Send;

end Cc51002_2;
