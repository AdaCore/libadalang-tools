PACKAGE BODY ibusiness_unit IS

    FUNCTION is_running (dev : IN object'class) RETURN boolean IS
    BEGIN
        RETURN dev.is_running;
    END is_running;

END ibusiness_unit;

