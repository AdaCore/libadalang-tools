PACKAGE ibusiness_unit IS

    TYPE object IS ABSTRACT TAGGED PRIVATE;
    TYPE reference IS ACCESS ALL object'class;
    TYPE view IS ACCESS CONSTANT object'class;

    nil : CONSTANT view := NULL;

    PROCEDURE finalize (obj : IN OUT object'class) IS ABSTRACT;
    -- 
    -- Prepares the object to be destroyed.

    FUNCTION is_running (dev : IN object'class) RETURN boolean;
    --
    -- Returns true if the device is running.

PRIVATE

    TYPE object IS ABSTRACT TAGGED
        RECORD
            is_running : boolean := false;
        END RECORD;

END ibusiness_unit;

