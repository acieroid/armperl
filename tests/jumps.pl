# Example of using conditions and returns
# Expected output: 120

sub fac($n) {
  if ($n < 0) {
    return -1; # invalid argument
  } elsif ($n == 0) {
    return 1;
  } else {
    return n*fac($n - 1);
  }
}

print(fac(5));
