with F650a00.P, F650a00.S;
package C650a01_Alerts is
   Practice : aliased F650a00.P.Practice_Alert :=
     F650a00.P.Make_Alert_For_Time (8.0);
   Trial : aliased F650a00.P.Practice_Alert :=
     (F650a00.Alert with Status => F650a00.P.Real, Urgency => F650a00.P.Low);
   Special : aliased F650a00.S.Special_Alert :=
     F650a00.S.Make_Alert_For_Time (54.0);
end C650a01_Alerts;
