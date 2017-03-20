with Ca11014_4;               -- Points list.
with Ca11014_1;               -- Generic list operation.
pragma Elaborate (Ca11014_1);
package Ca11014_5 is new Ca11014_1 (Ca11014_4);         -- Scores list.
