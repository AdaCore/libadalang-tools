

PACKAGE BODY C94005B_PKG IS

     TASK BODY TT IS
          LOCAL : INTEGER;
     BEGIN
          ACCEPT E (I : INTEGER) DO
               LOCAL := I;
          END E;
          DELAY 60.0;    -- SINCE THE PARENT UNIT HAS HIGHER PRIORITY
                         -- AT THIS POINT, IT WILL RECEIVE CONTROL AND
                         -- TERMINATE IF THE ERROR IS PRESENT.
          GLOBAL := LOCAL;
     END TT;

END C94005B_PKG;
