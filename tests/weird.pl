# Weird things are allowed in Perl.
# This should print 1.
sub foo() {
  return 0;
}

sub bar() {
  return &foo(return 1);
}

print &bar();
