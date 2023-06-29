package body Test is

	--------------------
	--  Get_My_Result --
	--------------------

	overriding function Get_My_Result
	  (C : in My_Llllllllllllllllllong_Type) return My_Result
	is
		Result : My_Result;
	begin
		if C.Value /= Null_Result then
			Add (Result, C.Value);
		else
			Add
			  (Result,
				Get_Value_Dependencies
				  (W, Id => C.Value_Index, C_Id => Null_Result));
		end if;

		--   my comment line 11111111111111111111111
		--  line 2222222222222222222222222222222222222222222

		-- my fillable comment xxxxxxxxxxxxxxxxxxxxxxxxxx
		-- my comment's 2nd line yyyyyyyyyyyyyyyyyyy

		return Result;
	end Get_My_Result;

	------------------------
	--  Get_My_Result_Bis --
	------------------------

	function Get_My_Result_Bis (C : in My_Type) return My_Result is
		Result : My_Result;
	begin
		if C.Value /= Null_Result then
			Add (Result, C.Value);
		end if;

		--   bis my comment line 11111111111111111111111
		--  bis line 2222222222222222222222222222222222222222222

		-- bis my fillable comment xxxxxxxxxxxxxxxxxxxxxxxxxx
		-- bis my comment's 2nd line yyyyyyyyyyyyyyyyyyy

		return Result;
	end Get_My_Result_Bis;
end Test;
