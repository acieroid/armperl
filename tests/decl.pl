# Simple hello world with a function declaration
# Expected output:
# Hello, world!

sub hello($name) {
  print("Hello, " . $name . '
');
}

&hello("world!");
&hello("world!");
&hello("world!");
