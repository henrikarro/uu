fun gcd (0, n) = n
  | gcd (m, n) = gcd (n mod m, m);

fun sum [] = 0
  | sum (x::xs) = x + sum xs;

fun fac 0 = 1
  | fac n = n * fac (n-1);

fun fac n =
  if n < 0 then
    raise Domain
  else if n = 0 then
    1
  else
    n * fac (n-1);

fun fac n =
  let
    fun fac' 0 = 1
      | fac' n = n * fac (n-1)
  in
    if n < 0 then
      raise Domain
    else
      fac' n
  end;

fun expo x n =
  let
    fun expo' x 0 = 1
      | expo' x n = x * expo' x (n-1)
  in
    if n < 0 then
      raise Domain
    else
      expo' x n
  end

fun expo x n =
  let
    fun expo' 0 = 1
      | expo' n = x * expo' (n-1)
  in
    if n < 0 then
      raise Domain
    else
      expo' n
  end;

fun triangle a b =
  if a > b then
    0
  else
    a + triangle (a+1) b;

fun f 0 = 0
  | f n = 1 + f (n-1);

fun g 0 = 0
  | g n = n + f (n-1);

fun int_div a b =
  let
    fun int_div' a b =
      if a < b then
        (0, a)
      else
        let
          val (q, r) = int_div' (a-b) b
        in
          (q+1, r)
        end
  in
    if a < 0 orelse b <= 0 then
      raise Domain
    else
      int_div' a b
  end;

fun fib n =
  let
    fun fib' 0 = 1
      | fib' 1 = 1
      | fib' n = fib' (n-1) + fib' (n-2)
  in
    if n < 0 then
      raise Domain
    else
      fib' n
  end;

fun even 0 = true
  | even n = odd (n-1)
and odd 0 = false
  | odd n = even (n-1);

fun acker 0 m = m+1
  | acker n 0 = acker (n-1) 1
  | acker n m = acker (n-1) (acker n (m-1));

fun indivisible n low up =
  low > up orelse
    (n mod low <> 0 andalso indivisible n (low+1) up);

fun prime n =
  if n <= 0 then
    raise Domain
  else
    n > 1 andalso indivisible n 2 (n-1);
