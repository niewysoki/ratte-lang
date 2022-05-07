fn Main () -> Void {
  let num: Int = 10;
  let str: String = "abracadabra";
  let val: Boolean = True;

  let a: Int = 1;
  let b: Int = 2;
  let mut c: Int = 5;

  c = c + a;
  c++;
  c--;
  let d: Boolean = c < num;
  let mut e: Boolean = not d;
  e = e and d;
  e = e or d;

  return;
}