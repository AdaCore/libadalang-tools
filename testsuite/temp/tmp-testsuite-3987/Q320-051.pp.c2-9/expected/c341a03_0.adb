function C341a03_0
  (The_Account : Account_Type'Class) return F341a00_0.Dollar_Amount
is
   Acct : Account_Type'Class := The_Account;  -- Init. of class-wide with
begin                                         -- another class-wide object.
   return Acct.Current_Balance;
end C341a03_0;
