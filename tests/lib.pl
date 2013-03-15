# various tests of the standard library

sub display($x) {
  print($x);
  print("
");
}

$foo = 5;
if (defined($foo)) {
  display("should be printed (1)");
};
if (defined($bar)) {
  display("should not be printed (1)");
};

display("should be printed (2)");
display(42); # should be printed
display($bar); # should not be printed

display(length("foo")); # should print 3
display(length(123)); # should print 3
display(length($bar)); # should print 0

display(substr("foobar", 3)); # should print bar
display(substr("foobar", 1, 4)); # should print ooba
