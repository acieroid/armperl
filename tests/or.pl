# Test || and &&

if (1 == 0 || 0 == 0) {
  print("I should be printed (1)");
} else {
  print("I should not be printed (1)");
};

if (1 == 1 || 0 == 0) {
  print("I should be printed (2)");
} else {
  print("I should not be printed (2)");
};
