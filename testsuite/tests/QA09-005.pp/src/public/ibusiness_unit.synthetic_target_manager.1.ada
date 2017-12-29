PACKAGE ibusiness_unit.synthetic_target_manager IS

    TYPE object IS NEW ibusiness_unit.object WITH PRIVATE;
    TYPE reference IS ACCESS ALL object'class;
    TYPE view IS ACCESS CONSTANT object'class;

    nil : CONSTANT view := NULL;

------------------------------------------------------------------------------
-- Basic Methodes
-------------------------------------------------------------------------------

    FUNCTION create (tracer_class_name        : IN string := "";
                     tracer_class_description : IN string := "")
                    RETURN ibusiness_unit.reference;
    --
    -- Creates the object and initializes internal data structure.

    PROCEDURE initialize (obj                        : IN OUT object;
                          gunking_mode               : IN string;
                          synthetic_target_container : IN string;
                          setup_manager              : IN string;
                          hmi_communication          : IN string;
                          recording_manager          : IN string);
    -- 
    -- Initializes the object
  
    PROCEDURE finalize (ref : IN OUT reference);
    --
    -- Deallocates memory and deletes object instance.

    PROCEDURE proceed (obj : IN OUT object);
    --
    -- Call for cyclic processing


------------------------------------------------------------------------------
-- Basic Methodes
-------------------------------------------------------------------------------


   FUNCTION is_running (obj : IN object) RETURN boolean;
   --
   -- Returns true, if a synthetic flight is running.


PRIVATE

    TYPE private_members;
    TYPE access_private_members IS ACCESS private_members;
    TYPE object IS NEW ibusiness_unit.object WITH
        RECORD
            member : access_private_members;
        END RECORD;

END ibusiness_unit.synthetic_target_manager;

