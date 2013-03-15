# Test the binary and unary operations

# those lines shoud print 42
print("
Testing arithmetic operations (should be 42 everytime)
");
print(41+1);
print(43-1);
print(6*7);
print(84/2);

# this line should print foobar
print("
Testing concatenation (should be foobar)
");
print("foo" . "bar");

# those lines should print 1
print("
Testing comparisons (should be 1 everytime)
");
print(1 == 1);
print(2 != 1);
print(5 > 1);
print(5 < 12);
print(5 >= 5);
print(7 <= 8);
print("foo" eq "foo");
print("foo" ne "bar");
print("foo" gt "bar");
print("foo" ge "foo");
print("bar" lt "foo");
print("bar" le "foo");

print("
Testing unary operations (should be 1 everytime)
");
print(!0);
print(!"0");
print(!1 + 1);
print(not 0);
print(not "0");
print(1 + (not 0 + 1));
print(-(-1));
print(+1);
