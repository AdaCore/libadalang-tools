package Utils.Strings is

   function Has_Prefix (X, Prefix : String) return Boolean;
   --  True if Prefix is at the beginning of X. For example,
   --  Has_Prefix("An_Identifier", Prefix => "An_") is True.

   function Has_Suffix (X, Suffix : String) return Boolean;
   --  True if Suffix is at the end of X

   function Replace_String (S, From, To : String) return String;
   --  Replaces all occurrences of From in S with To

   function Dashes (S : String) return String is
     (Replace_String (S, From => "_", To => "-"));

end Utils.Strings;
