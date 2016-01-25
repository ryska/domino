# System Inteligentnego Domu


## Ogólny opis programu

W założeniu system ma pozwalać na zaprogramowanie prostych zależności pomiędzy różnymi nadajnikami i czujnikami rozmieszczonymi w domu. Aplikacja udostępnia jednolity interfejs programistyczny, dzięki któremu możemy zdefiniować _reakcje_ systemu na dane płynące z czujników.

### Architektura

Głównym, stałym, elementem systemu jest serwer, którego zadaniem jest rejestrowanie klientów oraz zarządzanie i wymiana danych. Jego działanie jest oparte o główny wątek obsługujący przychodzące połączenia i odbiera z nich dane. Odebrane informacje są następnie przekazanie do nowego wątku, który po ich analizie, podejmuje odpowiednie akcje.

Kolejną częścią aplikacji są małe skrypty klienckie pełniące rolę czujników oraz kontrolerów, które wymieniają się danymi z serwerem.

Komunikacja między powyższymi elementami przebiega przy pomocy protokołu UDP, który dobrze sprawdza się w przypadku przesyłania małych porcji danych w sieci.

## Obsługa programu

Ze względu na charakter systemu, główny aspekt obsługi odnosi się do programowania odpowiednich zależności w języku Erlang. Najważniejsze funkcje to:
  * `dom_server:start/1` - Pozwala na uruchomienie serwera na podanym porcie.
  * `dom_server:add_func/2` - Pozwala na dodanie funkcji, która będzie wywołana za każdym razem, kiedy klient o podanym ID przyśle nowe dane.
  * `dom_temp:start/4`, `dom_alarm:start/4`, `dom_dym:start/4`, `dom_okno:start/4`, `dom_sms:start/4` - Pozwalają na uruchomienie danego klienta o podanych danych i podłączenie go do serwera o podanym adresie.

W pliku `dom_func.elr` można znaleźć przykładowe, zaimplementowane przez nas funkcje, które można podać jako argumenty funkcjo `dom_server:add_func/2`.
### Informacje dodatkowe
W programie zostały użyte tylko wbudowane funkcje Erlanga. Do komunikacji między serwerem i klientami wykorzystane zostały funkcje z rodziny `gen_udp`.

## Możliwe rozszerzenia programu

Jednym z rozszerzeń programu jakie można by zaimplementować jest prosty interfejs graficzny, który pozwalałby na monitorowanie wartości danych czujników oraz stanu kontrolerów. Dodatkowym praktycznym udogodnieniem byłaby możliwość podzielenia urządzeń na kategorie odpowiadające na przykład pomieszczeniom, w których się znajdują.
