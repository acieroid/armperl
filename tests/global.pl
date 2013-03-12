# Example with global and local variables

sub fun($arg1, $arg2) {
  $arg1 = $global;
  $global = 0;
  return $arg1;
}

$global = 42;

fun(1, 2);
