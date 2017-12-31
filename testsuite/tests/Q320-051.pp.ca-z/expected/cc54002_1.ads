     --===================================================================--

with Cc54002_0;      -- Generic List of pointers.
pragma Elaborate (Cc54002_0);

package Cc54002_1 is

   subtype Lengths is Natural range 0 .. 50;

   type Subscriber (Nlen, Alen : Lengths := 50) is record
      Name    : String (1 .. Nlen);
      Address : String (1 .. Alen);
      -- ... Other components.
   end record;

   type Subscriber_Ptr is access all Subscriber;         -- General access-to-
   -- variable type.

   package District_Subscription_Lists is new Cc54002_0
     (Element_Type => Subscriber, Element_Ptr => Subscriber_Ptr, Size => 100);

   District_01_Subscribers : District_Subscription_Lists.List_Type;

   New_Subscriber_01 : aliased Cc54002_1.Subscriber :=
     (12, 23, "Brown, Silas", "King's Pyland, Dartmoor");

   New_Subscriber_02 : aliased Cc54002_1.Subscriber :=
     (16, 23, "Hatherly, Victor", "16A Victoria St. London");

end Cc54002_1;
