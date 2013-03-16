# Test of conditions

if (1 == 1) {
  print("I should be printed (1)");
} else {
  print("I should not be printed (1) ");
};

if (1 != 1) {
  print("I should not be printed (2)");
} elsif (2 == 3) {
  print("I should not be printed (3)");
} else {
  print("I should be printed (2)");
};

if (1 != 1) {
  print("I should not be printed (4)");
} elsif (2 != 3) {
  print("I should be printed (3)");
} else {
  print("I should not be printed (5)");
};

unless (1 == 1) {
  print("I should not be printed (1) ");
} else {
  print("I should be printed (1)");
};

unless (1 != 1) {
  print("I should be printed (2)");
} elsif (2 == 3) {
  print("I should not be printed (3)");
} else {
  print("I should not be printed (2)");
};

unless (1 == 1) {
  print("I should not be printed (4)");
} elsif (2 != 3) {
  print("I should be printed (3)");
} else {
  print("I should not be printed (5)");
};

