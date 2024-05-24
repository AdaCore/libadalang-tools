package Protec is

   task type My_Task is
      entry Bar (X : Integer);
   end My_Task;

   function Foo (X : Integer) return Integer;

end Protec;
