# various tests of the standard library

$foo = 5;
if (defined($foo)) {
  print("should be printed (1)");
};
if (defined($bar)) {
  print("should not be printed (1)");
};

print("should be printed (2)");
print(42); # should be printed
print($bar); # should not be printed

print(length("foo")); # should print 3
print(length(123)); # should print 3
print(length($bar)); # should print 0

print(substr("foobar", 3)); # should print bar
print(substr("foobar", 2, 5)); # should print ooba
