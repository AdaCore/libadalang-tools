with Priv.Child;

package Pub is
   type Pub_Rec is record
      Val : Priv.Child.R;
   end record;

   function Bar (X : Pub_Rec) return Integer is (Priv.Child.Foo (X.Val));
end Pub;
