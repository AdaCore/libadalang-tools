with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   procedure A;

   procedure A is
   begin
      Ada.Text_IO.Put_Line ("A");
   end A;

   type B is (L1, L2, L3);

   C1, C2 : B := L1;

   task D;

   task body D is
   begin
      Put_Line ("In task D");
   end D;

   task type E;

   task body E is
   begin
      Put_Line ("In task E");
   end E;

   procedure F;

   procedure F is
   begin
      null;
   end F;

   package G is
      type G_Type is (G1, G2, G3);
      procedure Print_G (G_Spec : G_Type);
   end G;

   package body G is
      procedure Print_G (G_Spec : G_Type) is
      begin
         null;
      end Print_G;
   end G;

   function H (Spec_1, Spec_2 : Integer; Spec_3 : Float) return Boolean;

   function H (Spec_1, Spec_2 : Integer; Spec_3 : Float) return Boolean is
   begin
      return True;
   end H;

   function I (Spec_1, Spec_2 : Integer; Spec_3 : Float) return Boolean;

   function I (Spec_1, Spec_2 : Integer; Spec_3 : Float) return Boolean is
   begin
      return True;
   end I;

   function J (JSpec_1: Integer; JSpec_2 : Integer; JSpec_3 : Float)
               return Boolean;

   function J (JSpec_1: Integer; JSpec_2 : Integer; JSpec_3 : Float)
               return Boolean is
   begin
      return True;
   end J;

   function K (Spec_1: Integer; Spec_2 : Float)
               return Boolean;

   function K (Spec_1: Integer; Spec_2 : Float)
               return Boolean is
   begin
      return True;
   end K;

begin
   null;
end Main;
