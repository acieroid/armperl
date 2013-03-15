# Example with global and local variables
# Expected output: 42 5

sub fun($arg1, $arg2) {
  $arg1 = $global;
  $global = 5;
  return $arg1;
}

$global = 42;

print(fun(1, 2) . " " . $global);
