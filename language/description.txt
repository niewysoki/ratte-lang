Język jest bardzo zbliżony do Latte, z zapożyczeniami składniowymi z Rust i Pythona.
Opis punktów z tabelki:
01 - Int, String, Boolean, istnieje również typ Void
02 - Operatory logiczne ||, &&, !, zostały zastąpione (jak w Pythonie)
  angielskimi słowami or, and, not, aby poprawić czytelność kodu.
03 - Stałe tworzy się w następujący sposób:
  let %nazwa% : %typ% = wartość ;
  let %nazwa% = wartość ;
  zmienne natomiast:
  let mut %nazwa% : %typ% = wartość ;
  let mut %nazwa% = wartość ;
04 - Wypisywanie odbywa się przy pomocy funkcji Println,
  która przyjmuje zmienną typu String, wypisuje napis i znak nowej linii.
  Aby wypisać wartość Int, bądź Boolean, należy wywołać funkcję ShowInt, bądź ShowBoolean i jej
  wynik podać funkcji Println.
05 - Nawiasy {} są konieczne do bloków przy instrukcjach, if, else, while
06 - Funkcje definiuje się jak w Rust:
  fn %nazwa% (%list argumentów%) -> %typ% {...}
07 - Argumenty funkcji są domyślnie przekazywane przez
  wartość i postaci %nazwa% : %typ%. Jeśli chcemy przekazać
  argument przez referencję, to musimy napisać
  mut %nazwa% : %typ%.
12 - Język jest statycznie typowany.
13 - Funkcje mogą być zagnieżdżane . Funkcje zagnieżdżone mogą przesłaniać funkcje
  z wyższych poziomów (w tym funkcję w ramach której jest zdefiniowana zagnieżdżona funkcja).
17 - Zmienne i argumenty funkcji mogą być typu funkcyjnego.
  Wygląda on jak w Rust: fn (%list typów argumentów%) -> %typ%.
  Lambdy wyglądają następująco:
  | %lista argumentów% | -> %typ% {...} ;
  typ argumentu przekazującego wartość jest postaci %typ%
  typ argumentu przekazującego referencję jest postaci %mut typ%
