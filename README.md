# ADA projekt#1 2023 Informatyka semestr 3 - języki programowania
Polecenie:
ADA - zadanie
Podziel swój ID studenta przez 3 – reszta z dzielenia określa
rodzaj spotkania, które musi być użyte w rozwiązaniu:
0 -> spotkania selektywne (selective accept na
diagramach składniowych)
1 -> czasomierz z budzikiem (timed entry call)
2 -> spotkanie warunkowe (conditional entry call)
Opis zadania
1. Dostarczony program przykładowy Ady zawiera zadania (task) producentów, konsumentów i
zadanie bufora. Można go dowolnie zmieniać i dostosowywać do własnego zadania. Program
przykładowy nie zawiera elementów synchronizacji (instrukcji select oraz innych warunków czy
dozorów).
2. Kiedy do magazynu nie można wstawić wyrobu, jest on tracony, a jeśli nie można pobrać zestawu,
wysyłany jest pusty zestaw o numerze 0 - w gotowym programie tak być nie może
3. W programie musi być wykorzystywany wylosowany mechanizm chociaż jeden raz (zauważ, że
wylosowany mechanizm może nie pasować dobrze do każdej sytuacji).
4. Do zadania należy wymyślić własną tematykę - jest wiele możliwości, np.:
◦ produkcja czegoś, np. samochodów, telewizorów, mebli itp.
◦ usługi, np. stołówka/restauracja, pralnia itp.
◦ komunikacja (protokoły)
◦ transport, np. pociągi, tramwaje na torach, startujące samoloty itp.
Wymagania względem zadania
Program powinien:
1. zawierać wylosowany mechanizm i pokazywać jego użycie (instrukcja select dostosowana do problemu),
2. wypisywanych komunikatów, które wyjaśniają, co się dzieje w programie (np. piekę ciastko nr 7) i jak
przebiega sterowanie, szczególnie w wylosowanym mechanizmie synchronizacji.
3. być opracowany samodzielnie w domu,
4. zostać dostarczony prowadzącemu korzystając z platformy Moodle najpóźniej dzień przez zajęciami
5. Program będzie później modyfikowany na zajęciach i modyfikacja będzie musiała być przesłana przed
zakończeniem tych zajęć
Zasada działania programu
Producenci wytwarzają swój produkt w losowych
chwilach i wysyłają do bufora (dany producent
wytwarza jeden rodzaj produktu)
Jeśli jest miejsce – bufor przyjmuje ten produkt
Konsument składa w losowych chwilach
zamówienie na wyrób, który jest zestawem kilku
produktów – w przykładowym programie są
zdefiniowane trzy zestawy i każdy konsument może
zamówić każdy z tych zestawów
Bufor wydaje zestaw, gdy ma odpowiednią liczbę
produktów, aby go wytworzyć. Po wydaniu
zestawu, jego składniki znikają z bufora
bufor
producent 1
producent 2
producent 3
konsument 1
konsument 2
Problemy do rozwiązania
Aktualnie, gdy producent nie może umieścić produktu w buforze, produkt znika, konsument
natomiast dostaje zestaw nr 0, gdy nie można wydać zestawu – należy to poprawić tak, by miało
sens z wybranym tematem
W buforze może dojść do zakleszczenia – np. gdy bufor będzie pełny, ale nie będzie w nim
produktów tworzących zestaw (np. na wcześniejszym rysunku byłyby same trójkąty) – należy
przewidzieć taką sytuację i jej zapobiec
Przy okazji warto zwrócić uwagę, czy jakieś procesy nie są „głodzone”, np. gdybyśmy wprowadzili
zbyt duże ograniczenie na przyjmowanie żółtego kwadratu, to byłby problem z otrzymaniem
zestawów zawierających kwadrat. Natomiast zbyt mała restrykcja mogłaby sprawić, że bufor
zapełni się kwadratami, na które jest najmniejszy popyt.
Należy starać się jak najmniej spowalniać system
Co ma wpływ na ocenę?
Termin oddania
Program należy umieć uruchomić i powinien się kompilować
Punkty ujemne można dostać np. za:
◦ brak przejścia przez różne gałęzie instrukcji select,
◦ brak komunikatów objaśniających działanie programu, w tym przejścia przez różne gałęzie,
◦ zbyt wolne lub zbyt szybkie wyświetlanie komunikatów w sposób utrudniający sprawdzenie programu,
◦ błędną synchronizację, np. gubienie elementów lub niepotrzebne spowalnianie systemu.
