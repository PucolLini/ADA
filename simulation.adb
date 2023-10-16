-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there

--Patrycja Nerc 193075
--Karina Woloszyn 193592

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Simulation is
   Number_Of_Products   : constant Integer := 5;
   Number_Of_Assemblies : constant Integer := 4;
   Number_Of_Consumers  : constant Integer := 4;

   Can_Accept_Assembly  : Boolean := True;

   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;

      subtype Production_Time_Range is Integer range 3 .. 5;
      subtype Production_Time_Range2 is Integer range 1 .. 4;
      subtype Production_Range is Integer range 1 .. 2 ;

   Product_Name : constant array (Product_Type) of String (1 .. 14) :=
     ("Sensors       ", 
      "Wheels        ", 
      "Breaks        ", 
      "Steering Wheel",
      "Lamps         ");
   Assembly_Name : constant array (Assembly_Type) of String (1 .. 17) :=
     ("Safety           ", 
      "Steering         ", 
      "Drive            ", 
      "Exterior elements");

   package Random_Assembly is new Ada.Numerics.Discrete_Random (Assembly_Type);
   type My_Str is new String (1 .. 256);

   task type Producer is
      entry Start (Product : in Product_Type; Production_Time : in Integer);
   end Producer;

   task type Consumer is
      entry Start
        (Consumer_Number : in Consumer_Type; Consumption_Time : in Integer);
   end Consumer;

   task type Buffer is
      entry Take (Product : in Product_Type; Number : in Integer);
      entry Deliver (Assembly : in Assembly_Type; Number : out Integer);
   end Buffer;

   P : array (1 .. Number_Of_Products) of Producer;
   K : array (1 .. Number_Of_Consumers) of Consumer;
   B : Buffer;

   task body Producer is

      package Random_Production is new Ada.Numerics.Discrete_Random
        (Production_Time_Range);
      package Random_Production_1to3 is new Ada.Numerics.Discrete_Random
        (Production_Time_Range2);
      package Random_Range is new Ada.Numerics.Discrete_Random
        (Production_Range);
      G  : Random_Production.Generator;   --  generator liczb losowych
      G2 : Random_Production_1to3.Generator;
      G3 : Random_Range.Generator;
      Product_Type_Number : Integer;
      Product_Type_Number_1to3 : Integer;
      Product_Number      : Integer;
      Production          : Integer;
      Random_Number		   : Integer;
      Product_Before      : Product_Type := Product_Type(Number_Of_Products);
      
      procedure Dostawa is
      begin
         Random_Production.Reset (G);
         Random_Production_1to3.Reset (G2);
         Random_Range.Reset (G3);
         Random_Number := Random_Range.Random (G3);
         Put_Line("Dostawa started");
         

         if Random_Number = 1 then
            loop
               Product_Type_Number_1to3 := Random_Production_1to3.Random (G2);
               exit when Product_Type_Number_1to3 /= Product_Before;
            end loop;
            Put_Line ("Produced product " & Product_Name (Product_Type_Number_1to3));
            Product_Before := Product_Type_Number_1to3;
         else
            loop
               Product_Type_Number := Random_Production.Random(G);
               exit when Product_Type_Number /= Product_Before;
            end loop;
            
            Product_Before := Product_Type_Number;
            Put_Line
              ("Produced product " & Product_Name (Product_Type_Number));
            B.Take (Product_Type_Number, Product_Number);
         end if;

         Product_Number := Product_Number + 1;
         
      end Dostawa;

   begin
      accept Start (Product : in Product_Type; Production_Time : in Integer) do
         Random_Production.Reset (G);    --  start random number generator
         Random_Production_1to3.Reset (G2);
         Product_Number      := 1;
         Product_Type_Number := Product;
         Production          := Production_Time;
      end Start;

      Put_Line ("Started producer of " & Product_Name (Product_Type_Number));
      B.Take (Product_Type_Number, Product_Number);

      loop
         delay Duration(Random_Production.Random(G));
   
         Put_Line("Produced product " & Product_Name (Product_Type_Number));
         B.Take (Product_Type_Number, Product_Number);
         Product_Number := Product_Number + 1;
         Dostawa;
     end loop;
   end Producer;

   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new Ada.Numerics.Discrete_Random
        (Consumption_Time_Range);
      package Random_Assembly is new Ada.Numerics.Discrete_Random
        (Assembly_Type);
      subtype Consumer_Number_Range is Integer range 1 .. 4;
      package Random_Consumer is new Ada.Numerics.Discrete_Random
        (Consumer_Number_Range);

      G : Random_Consumption.Generator;  --  random number generator (time)
      G2              : Random_Assembly.Generator;    --  also (assemblies)
      G3				 : Random_Consumer.Generator;
      Consumer_Nb     : Consumer_Type;
      Assembly_Number : Integer;
      Consumption     : Integer;
      Assembly_Type   : Integer;
      Consumer_Name : constant array
        (1 .. Number_Of_Consumers) of String (1 .. 4) :=
        ("Audi", "Opel", "Jeep", "Seat");
   begin
      accept Start(Consumer_Number : in Consumer_Type; Consumption_Time : in Integer)
      do
         Random_Consumption.Reset (G);   --  ustaw generator
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;  --do jest do czasomierza z budzikiem, czy o chuj chodzi?
      end Start;
      Put_Line ("Started consumer " & Consumer_Name (Consumer_Nb));
      Assembly_Number := 1;
      loop
         delay Duration (Random_Consumption.Random (G));
         Random_Assembly.Reset(G2);
         Random_Consumer.Reset(G3);
         Assembly_Type := Random_Assembly.Random (G2);
         Consumer_Nb := Random_Consumer.Random(G3);
         B.Deliver (Assembly_Type, Assembly_Number);

         if Can_Accept_Assembly then
            Put_Line(Consumer_Name (Consumer_Nb) &
                          ": taken assembly " &
                       Assembly_Name (Assembly_Type));
            --show storage
         else
            Put_Line(Consumer_Name (Consumer_Nb) & 
                       " left - there was no " & 
                       Assembly_Name (Assembly_Type));
         end if;
      end loop;
   end Consumer;

   task body Buffer is
      Storage_Capacity : constant Integer := 50;--zwiekszy
      type Storage_type is array (Product_Type) of Integer;
      Storage          : Storage_type := (0, 0, 0, 0, 0);
      Assembly_Content : array (Assembly_Type, Product_Type) of Integer :=
     --  cz,ko,ha,ki,la
        ((1, 0, 2, 0, 1), --bezpieczenstwo
         (0, 2, 1, 1, 0), -- sterowanie
         (1, 2, 0, 0, 0), -- naped
         (2, 0, 0, 0, 2) -- elem. zewnet.
        );
      Max_Assembly_Content : array (Product_Type) of Integer
		:= (12, 12, 9, 5, 9);
      Assembly_Number      : array (Assembly_Type) of Integer := (1, 1, 1, 1);
      In_Storage           : Integer                          := 0;

      procedure Setup_Variables is
      begin
         for W in Product_Type loop
           -- Max_Assembly_Content (W) := 0;
            for Z in Assembly_Type loop
              if Assembly_Content (Z, W) > Max_Assembly_Content (W) then
                  Max_Assembly_Content (W) := Assembly_Content (Z, W);
                  Put_Line("Setup: max asse " & Integer'Image( Max_Assembly_Content (W)));
                  Put_Line("Assembly_Content " & Integer'Image( Assembly_Content (Z, W)));
              end if;
            end loop;
         end loop;
      end Setup_Variables;

      function Can_Accept (Product : Product_Type) return Boolean is
         Free : Integer;         --  free room in the storage
         -- how many products are for production of arbitrary assembly
         Lacking : array (Product_Type) of Integer;
         -- how much room is needed in storage to produce arbitrary assembly
         Lacking_room : Integer;
         MP           : Boolean;                   --  can accept
      begin
         if In_Storage >= Storage_Capacity then
            return False;
         end if;
         Free := Storage_Capacity - In_Storage;
         MP   := True;
         for W in Product_Type loop
            if Storage (W) < Max_Assembly_Content (W) then
               MP := False;
            end if;
         end loop;
         if MP then
            return True;
         end if;
         if Storage (Product) = Max_Assembly_Content (Product) then
               Put_Line("NO SPACE IN STORAGE FOR " & Product_Name(Product)); 
               Put_Line("STORAGE: " & Integer'Image(Storage(Product)) & " MAX: " & Integer'Image(Max_Assembly_Content(Product)));
               MP := False;
               return False;
         end if;
         if Integer'Max(0, Max_Assembly_Content (Product) - Storage (Product)) > 0
         then
            return True;
         end if;
         
         Lacking_room := 1;  --  insert current product
         
         for W in Product_Type loop
            Lacking (W) := Integer'Max (0, Max_Assembly_Content (W) - Storage (W));
            Lacking_room := Lacking_room + Lacking (W);
         end loop;
         if Free >= Lacking_room then
            -- there is enough room in storage for arbitrary assembly
            return True;
         else
            Put_Line("NO SPACE IN STORAGE FOR " & Product_Name(Product)); 
            Put_Line("STORAGE: " & Integer'Image(Storage(Product)) & " MAX: " & Integer'Image(Max_Assembly_Content(Product))); 
            return False;
         end if;
      end Can_Accept;

      function Can_Deliver (Assembly : Assembly_Type) return Boolean is
      begin
         for W in Product_Type loop
            if Storage (W) < Assembly_Content (Assembly, W) then
               Can_Accept_Assembly := False;
               return False;
            end if;
         end loop;
         Can_Accept_Assembly := True;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         Put_Line("+++++++++++++++++++++++++");
         Put_Line("Storage contents: ");
         for W in Product_Type loop
            Put_Line ( Product_Name (W)& " x" & Integer'Image (Storage (W)));
         end loop;
         Put_Line("+++++++++++++++++++++++++");
      end Storage_Contents;

      procedure IsInBufer (Assembly :  in Assembly_Type; Number : out Integer) is
      begin
          Put_Line("Delivered assembly " & Assembly_Name (Assembly));
          for W in Product_Type loop
                     Storage (W) := Storage (W) - Assembly_Content (Assembly, W);
                     In_Storage  := In_Storage - Assembly_Content (Assembly, W);
          end loop;

          Number                     := Assembly_Number (Assembly);
          Assembly_Number (Assembly) := Assembly_Number (Assembly) + 1;
          --Storage_Contents;
      end IsInBufer;
      
   begin
      Put_Line ("Buffer started");
      Setup_Variables;
      loop
         select
            accept Take (Product : in Product_Type; Number : in Integer) do
               Put_Line("Take started " & Product_Name (Product));
               if Can_Accept (Product) then
                  Put_Line
                    ("Accepted product " & Product_Name (Product));
                  Storage (Product) := Storage (Product) + 1;
                  In_Storage        := In_Storage + 1;
                  ---Storage_Contents;
               else
                  Put_Line
                    ("Rejected product " & Product_Name (Product));
               end if;
            end Take;
            or
             accept Deliver (Assembly : in Assembly_Type; Number : out Integer) do
               Put_Line("Deliver started " & Assembly_Name (Assembly));
               if Can_Deliver (Assembly) then
                  IsInBufer(Assembly, Number);
                  Can_Accept_Assembly := True;
               else
                  Put_Line("There aren't enough products - please wait");
                  for I in 1 .. 4 loop
                     Put_Line("Try no. " & Integer'Image(I));
                     if Can_Deliver (Assembly) then
                        IsInBufer(Assembly, Number);
                        exit;
                     else
                         Put_Line("Please wait");
                        delay 3.0;
                     end if;
                  end loop;
                  Put_Line("Lacking products for assembly " & Assembly_Name (Assembly));
                  Number := 0;
               end if;
                  --do tablicy braków dopisywane są produkty i klient czeka na dostaw
            end Deliver;
         end select;
         Storage_Contents;
      end loop;
      
   end Buffer;

begin
   for I in 1 .. Number_Of_Products loop
      P (I).Start (I, 8);
   end loop;

   for J in 1 .. Number_Of_Consumers loop
      K (J).Start (J, 12);
   end loop;
end Simulation;
