WITH Ada.Unchecked_Deallocation;

PACKAGE BODY ibusiness_unit.synthetic_target_manager IS


   TYPE private_members IS RECORD


      is_initialized : Boolean := False;
      --
      -- Object state elements


   END RECORD;


   ----------------------------------------------------------------------
   -- Deallocation Methodes
   ----------------------------------------------------------------------

   PROCEDURE free IS NEW Ada.Unchecked_Deallocation (Object => object'class, Name => reference);
   PROCEDURE free IS NEW Ada.Unchecked_Deallocation (private_members, access_private_members);

   ----------------------------------------------------------------------
   -- Interface Management Methodes
   ----------------------------------------------------------------------

   FUNCTION create
     (tracer_class_name        : IN String := "";
      tracer_class_description : IN String := "") RETURN ibusiness_unit.reference IS
      myself : reference;

   BEGIN
      -- Create myself
      --
      myself        := NEW object;
      myself.member := NEW private_members;

      myself.is_running := True;

      RETURN ibusiness_unit.reference (myself);

   EXCEPTION
      WHEN error : OTHERS =>
         RETURN NULL;
   END create;

   PROCEDURE initialize
     (obj                        : IN OUT object;
      gunking_mode               : IN     String;
      synthetic_target_container : IN     String;
      setup_manager              : IN     String;
      hmi_communication          : IN     String;
      recording_manager          : IN     String) IS

      pm : private_members RENAMES obj.member.ALL;

   --  result : rec.register_result;

   BEGIN

      IF NOT pm.is_initialized THEN


         pm.is_initialized := True;

      END IF;


   EXCEPTION
      WHEN error : OTHERS =>
         NULL;
   END initialize;

   PROCEDURE finalize (ref : IN OUT reference) IS
   BEGIN



      IF ref /= NULL THEN

         -- Kill local member
         --
         free (ref.member);

         -- Kill myself
         --
         free (ref);

      END IF;



   EXCEPTION
      WHEN error : OTHERS =>
         NULL;
   END finalize;

   ----------------------------------------------------------------------
   -- Interface Proceed Methodes
   ----------------------------------------------------------------------

   PROCEDURE proceed (obj : IN OUT object) IS

      pm : private_members RENAMES obj.member.ALL;

   BEGIN

      NULL;


   EXCEPTION
      WHEN error : OTHERS =>
         NULL;
   END proceed;

   ------------------------------------------------------------------------------
   -- Basic Methodes
   -------------------------------------------------------------------------------

   FUNCTION is_running (obj : IN object) RETURN Boolean IS
   BEGIN
      RETURN True;
   END is_running;

END ibusiness_unit.synthetic_target_manager;
