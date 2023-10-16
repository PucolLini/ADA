-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Simulation is
   Number_Of_Products   : constant Integer := 11;
   Number_Of_Assemblies : constant Integer := 5;
   Number_Of_Consumers  : constant Integer := 4;

   Can_Accept_Assembly  : Boolean := True;

   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;

      subtype Production_Time_Range is Integer range 3 .. 11;
      subtype Production_Time_Range2 is Integer range 1 .. 4;
      subtype Production_Range is Integer range 1 .. 2 ;

   Product_Name : constant array (Product_Type) of String (1 .. 20) :=
     ("-----Czujniki-------", "--------Kola--------", "-------Szyby--------",
      "------Hamulce-------", "-----Kierownica-----", "----Klimatyzacja----",
      "-Kratki wentylacyjne", "-------Lampy--------", "-------Lustrka------",
      "-Poduszki powietrza-",  "----Wycieraczki-----");
   Assembly_Name : constant array (Assembly_Type) of String (1 .. 20) :=
     ("--BezpieczeÅstwo---", "----Sterowanie------", "-----NapÄÂd-------",
      "----Chlodzenie-----", "Elementy zewnetrzne");

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
      Random_Number		  : Integer;

      procedure Dostawa is
      begin
         Random_Production.Reset (G);
         Random_Production_1to3.Reset (G2);
         Random_Range.Reset (G3);
         Random_Number := Random_Range.Random (G3);
         Put_Line("Dostawa started");

         if Random_Number = 1 then
            Product_Type_Number_1to3 := Random_Production_1to3.Random (G2);
             Put_Line
              ("Produced product " & Product_Name (Product_Type_Number_1to3) &
              " number " & Integer'Image (Product_Number));
            	B.Take (Product_Type_Number_1to3, Product_Number);
         else
            Product_Type_Number := Random_Production.Random(G);
            Put_Line
              ("Produced product " & Product_Name (Product_Type_Number) &
                 " number " & Integer'Image (Product_Number));
            B.Take (Product_Type_Number, Product_Number);
         end if;

         Product_Number := Product_Number + 1;
      end Dostawa;

   begin
      Put_Line("Start begin");
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
         Put_Line("dostawa start");
         Put_Line("Produced product " & Product_Name (Product_Type_Number) &
                 " number " & Integer'Image (Product_Number));
         B.Take (Product_Type_Number, Product_Number);
         Product_Number := Product_Number + 1;
         --Dostawa;
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
      Put_Line("Costumer started begin");
      accept Start(Consumer_Number : in Consumer_Type; Consumption_Time : in Integer)
      do
         Random_Consumption.Reset (G);   --  ustaw generator
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;  --do jest do czasomierza z budzikiem, czy o chuj chodzi?
      end Start;
      Put_Line ("Started consumer " & Consumer_Name (Consumer_Nb));
      Assembly_Number := 1;
      loop
         Put_Line("Loop in costumer started");
         delay Duration (Random_Consumption.Random (G));
         Random_Assembly.Reset(G2);
         Random_Consumer.Reset(G3);
         Assembly_Type := Random_Assembly.Random (G2);
         Consumer_Nb := Random_Consumer.Random(G3);
         B.Deliver (Assembly_Type, Assembly_Number);
         --select
         if Can_Accept_Assembly then
            --select
               Put_Line(Consumer_Name (Consumer_Nb) &
                          ": taken assembly " &
                          Assembly_Name (Assembly_Type));
            --or delay 5.0;
            --end select;
         else
            Put_Line("Klient wyszedl");
         end if;
      end loop;
   end Consumer;

   task body Buffer is
      Storage_Capacity : constant Integer := 150;--zwiekszy
      type Storage_type is array (Product_Type) of Integer;
      Storage          : Storage_type := (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
      Assembly_Content : array (Assembly_Type, Product_Type) of Integer :=
      --  cz,ko,sz,ha,ki,kl,kr,la,lu,po,wy
        ((1, 0, 0, 2, 0, 0, 0, 1, 1, 2, 2), --bezpieczenstwo
         (1, 4, 0, 1, 1, 0, 0, 0, 0, 0, 0), -- sterowanie
         (1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0), -- naped
         (1, 0, 2, 0, 0, 1, 3, 0, 0, 0, 0), -- chlodzenie
         (2, 2, 2, 0, 0, 0, 0, 4, 2, 0, 3) -- elem. zewnet.
        ); -- przsunÄÄ szyby
      Max_Assembly_Content : array (Product_Type) of Integer
		:= (15, 20, 15, 10, 5, 5, 8, 12, 10, 8, 10);
      Assembly_Number      : array (Assembly_Type) of Integer := (1, 1, 1, 1, 1);
      In_Storage           : Integer                          := 0;

      procedure Setup_Variables is
      begin
         for W in Product_Type loop
           -- Max_Assembly_Content (W) := 0;
            for Z in Assembly_Type loop
             --  if Assembly_Content (Z, W) > Max_Assembly_Content (W) then
               --   Max_Assembly_Content (W) := Assembly_Content (Z, W);
                  Put_Line("Setup: max asse " & Integer'Image( Max_Assembly_Content (W)));
                  Put_Line("Assembly_Content " & Integer'Image( Assembly_Content (Z, W)));
              -- end if;
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
         Put_Line("Can_accept started" & Product_Name(Product));
         if In_Storage >= Storage_Capacity then
            return False;
         end if;
         -- There is free room in the storage
         Free := Storage_Capacity - In_Storage;
         MP   := True;
         for W in Product_Type loop
            if Storage (W) < Max_Assembly_Content (W) then
               MP := False;
            end if;
         end loop;
         if MP then
            return True; --  storage has products for arbitrary
            --  assembly
         end if;
         if Integer'Max(0, Max_Assembly_Content (Product) - Storage (Product)) > 0
         then
            -- exactly this product lacks
			 -- czy iloÃÂÃÂÃÂÃÂ ktÃÂÃÂ³rÃÂÃÂ dostarczymy wystarczy aby wydaÃÂÃÂ zamÃÂÃÂ³weinie do klienta?
            return True;
         end if;
         Lacking_room := 1;                     --  insert current product
         for W in Product_Type loop
            Lacking (W) :=
              Integer'Max (0, Max_Assembly_Content (W) - Storage (W));
            Lacking_room := Lacking_room + Lacking (W);
         end loop;
         if Free >= Lacking_room then
            -- there is enough room in storage for arbitrary assembly
            return True;
         else
            -- no room for this product
            return False;
         end if;
      end Can_Accept;

      function Can_Deliver (Assembly : Assembly_Type) return Boolean is
      begin
         for W in Product_Type loop
            Put_Line("Schowek check: " & Integer'Image(Storage(W)));
            Put_Line("Produkty potrzebne check: " & Integer'Image(Assembly_Content (Assembly, W)));
            if Storage (W) < Assembly_Content (Assembly, W) then
               Put_Line("nie moze dostaczyc");
               Can_Accept_Assembly := False;
               return False;
            end if;
         end loop;
         Put_Line("moze dostarczyc");
         Can_Accept_Assembly := True;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         Put_Line("Storage contents: ");
         for W in Product_Type loop
            Put_Line ( Product_Name (W)& " x" & Integer'Image (Storage (W)));
         end loop;
      end Storage_Contents;

      procedure IsInBufer (Assembly :  in Assembly_Type; Number : out Integer) is
      begin
          Put_Line("Delivered assembly " & Assembly_Name (Assembly) &
                       " number " & Integer'Image (Assembly_Number (Assembly)));
          for W in Product_Type loop
                     Storage (W) := Storage (W) - Assembly_Content (Assembly, W);
                     In_Storage  := In_Storage - Assembly_Content (Assembly, W);
          end loop;

          Number                     := Assembly_Number (Assembly);
          Assembly_Number (Assembly) := Assembly_Number (Assembly) + 1;
          Storage_Contents;
      end IsInBufer;

   begin
      Put_Line ("Buffer started");
      Setup_Variables;
      loop
         select
            accept Take (Product : in Product_Type; Number : in Integer) do
               Put_Line("Take started" & Product_Name (Product));
               if Can_Accept (Product) then
                  Put_Line
                    ("Accepted product " & Product_Name (Product) & " number " &
                       Integer'Image (Number)); --konwersja & liczby number na stringa
                  Storage (Product) := Storage (Product) + 1;
                  In_Storage        := In_Storage + 1;
                  Storage_Contents;
               else
                  Put_Line
                    ("Rejected product " & Product_Name (Product) & " number " &
                       Integer'Image (Number));
               end if;
            end Take;
            or
             accept Deliver (Assembly : in Assembly_Type; Number : out Integer) do
               Put_Line("Deliver started " & Assembly_Name (Assembly));
               if Can_Deliver (Assembly) then
                  Put_Line("czy moze dostarczyc");
                  IsInBufer(Assembly, Number);
                  Can_Accept_Assembly := True;
               else
                  for I in 1 .. 4 loop
                     Put_Line("Petla probujaca " & Integer'Image(I));
                     if Can_Deliver (Assembly) then
                        Put_Line("Proba " & Integer'Image(I));
                        IsInBufer(Assembly, Number);
                        exit;
                     else
                        delay 3.0;
                     end if;
                  end loop;
                  Put_Line("Lacking products for assembly " & Assembly_Name (Assembly));
                  Number := 0;
               end if;
                  --do tablicy brakÃ³w dopisywane sÄ produkty i klient czeka na dostaw
            end Deliver;
         end select;
      end loop;
   end Buffer;

begin
   for I in 1 .. Number_Of_Products loop
      Put_Line("Loop1 in last main started");
      P (I).Start (I, 2);  -- iteracja po tablicy produktÃÂÃÂ³w
      --delay 1.0;
   end loop;

   Put_Line("Loop1 ended");
   delay 10.0;

   for J in 1 .. Number_Of_Consumers loop
       Put_Line("Loop2 in last main started");
      K (J).Start (J, 12); -- iteracja po tablicy konsumentÃÂÃÂ³w
   end loop;
end Simulation;
