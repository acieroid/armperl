#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*********** Type stuff **** ***************************/
#define IS_STRING(ptr) ((((int) (ptr)) & 0x3) == 0)
#define IS_NUMBER(ptr) ((((int) (ptr)) & 0x1) == 1)
#define IS_UNDEF(ptr)  ((((int) (ptr)) & 0x3) == 2)

enum {
  STRING = 0,
  NUMBER = 1,
  UNDEF = 2
};

/* return the type of the value stored in the argument */
static int type_of(void *ptr) {
  if (IS_STRING(ptr)) {
    return STRING;
  } else if (IS_NUMBER(ptr)) {
    return NUMBER;
  } else if (IS_UNDEF(ptr)) {
    return UNDEF;
  } else {
    /* should not happen */
    return STRING;
  }
}


/*********** Helper functions ***************************/
/* convert a native integer to a perl value */
static void *box_int(int x)
{
  return (void *) ((x << 1) + 1);
}

/* convert a boxed integer to a native int */
static int unbox_int(int x)
{
  return (x >> 1);
}

/* return the size of an integer (in number of characters needed) */
static int intlen(int n)
{
  int i = 0;
  for (; n > 10; i++) {
    n /= 10;
  }
  return i+1;
}

/* convert a perl value to a native int */
static int to_native_int(void *x)
{
  int n = 0;
  switch (type_of(x)) {
  case NUMBER:
    return unbox_int((int) x);
  case UNDEF:
    return 0;
  case STRING:
    sscanf((char *) x, "%d", &n);
    return n;
  }
  return n;
}

/* convert a perl value to a native string. The string should be
   deallocated when it is not needed anymore */
static char *to_native_string(void *x)
{
  char *dst = NULL;
  int n = 0;
  switch (type_of(x)) {
  case STRING:
    dst = malloc(strlen(x)*sizeof(*dst));
    strcpy(dst, x);
    break;
  case UNDEF:
    dst = malloc(sizeof(*dst));
    dst[0] = '\0';
    break;
  case NUMBER:
    n = to_native_int(x);
    dst = malloc((intlen(n)+1)*sizeof(*dst));
    sprintf(dst, "%d", n);
    break;
  }
  return dst;
}


/****************** Operators ***************************/
void *perl_plus(void *x, void *y)
{
  return box_int(to_native_int(x) + to_native_int(y));
}

void *perl_minus(void *x, void *y)
{
  return box_int(to_native_int(x) - to_native_int(y));
}

void *perl_times(void *x, void *y)
{
  return box_int(to_native_int(x) * to_native_int(y));
}

void *perl_divide(void *x, void *y)
{
  return box_int(to_native_int(x) / to_native_int(y));
}

void *perl_concat(void *x, void *y)
{
  char *s1, *s2, *res;
  s1 = to_native_string(x);
  s2 = to_native_string(y);
  res = malloc((strlen(s1) + strlen(s2))*sizeof(*res));
  sprintf(res, "%s%s", s1, s2);
  free(s1);
  free(s2);
  return (void *)res;
}

void *perl_equals(void *x, void *y)
{
  int res;
  if (to_native_int(x) == to_native_int(y)) {
    res = 1;
  } else {
    res = 0;
  }
  return box_int(res);
}

void *perl_different(void *x, void *y)
{
  int res;
  if (to_native_int(x) != to_native_int(y)) {
    res = 1;
  } else {
    res = 0;
  }
  return box_int(res);
}

void *perl_greater(void *x, void *y)
{
  int res;
  if (to_native_int(x) > to_native_int(y)) {
    res = 1;
  } else {
    res = 0;
  }
  return box_int(res);
}

void *perl_lower(void *x, void *y)
{
  int res;
  if (to_native_int(x) < to_native_int(y)) {
    res = 1;
  } else {
    res = 0;
  }
  return box_int(res);
}

void *perl_greater_equals(void *x, void *y)
{
  int res;
  if (to_native_int(x) >= to_native_int(y)) {
    res = 1;
  } else {
    res = 0;
  }
  return box_int(res);
}

void *perl_lower_equals(void *x, void *y)
{
  int res;
  if (to_native_int(x) <= to_native_int(y)) {
    res = 1;
  } else {
    res = 0;
  }
  return box_int(res);
}

void *perl_str_equals(void *x, void *y)
{
  int res;
  char *s1 = to_native_string(x);
  char *s2 = to_native_string(y);
  if (strcmp(s1, s2) == 0) {
    res = 1;
  } else {
    res = 0;
  }
  free(s1);
  free(s2);
  return box_int(res);
}

void *perl_str_different(void *x, void *y)
{
  int res;
  char *s1 = to_native_string(x);
  char *s2 = to_native_string(y);
  if (strcmp(s1, s2) != 0) {
    res = 1;
  } else {
    res = 0;
  }
  free(s1);
  free(s2);
  return box_int(res);
}

void *perl_str_greater(void *x, void *y)
{
  int res;
  char *s1 = to_native_string(x);
  char *s2 = to_native_string(y);
  if (strcmp(s1, s2) > 0) {
    res = 1;
  } else {
    res = 0;
  }
  free(s1);
  free(s2);
  return box_int(res);
}

void *perl_str_lower(void *x, void *y)
{
  int res;
  char *s1 = to_native_string(x);
  char *s2 = to_native_string(y);
  if (strcmp(s1, s2) < 0) {
    res = 1;
  } else {
    res = 0;
  }
  free(s1);
  free(s2);
  return box_int(res);
}

void *perl_str_greater_equals(void *x, void *y)
{
  int res;
  char *s1 = to_native_string(x);
  char *s2 = to_native_string(y);
  if (strcmp(s1, s2) >= 0) {
    res = 1;
  } else {
    res = 0;
  }
  free(s1);
  free(s2);
  return box_int(res);
}


void *perl_str_lower_equals(void *x, void *y)
{
  int res;
  char *s1 = to_native_string(x);
  char *s2 = to_native_string(y);
  if (strcmp(s1, s2) <= 0) {
    res = 1;
  } else {
    res = 0;
  }
  free(s1);
  free(s2);
  return box_int(res);
}

void *perl_not(void *x)
{
  int value = to_native_int(x);
  int res = (value == 0);
  return box_int(res);
}

/*********** Standard functions *************************/
void *perl_fun_defined(void *arg)
{
  int res = !IS_UNDEF(arg);
  return box_int(res);
}

void *perl_fun_print(void *arg)
{
  switch (type_of(arg)) {
  case STRING:
    printf("%s", (char *) arg);
    break;
  case NUMBER:
    printf("%d", to_native_int(arg));
    break;
  default:
    break;
  }
  return NULL;
}

void *perl_fun_length(void *arg)
{
  int len = 0;
  switch (type_of(arg)) {
  case STRING:
    len = strlen(arg);
    break;
  case UNDEF:
    len = 0;
    break;
  case NUMBER:
    len = intlen(to_native_int(arg));
    break;
  }
  return box_int(len);
}

void *perl_fun_scalar(void *arg)
{
  /* Since we only have scalar values, scalar actually does nothing */
  return arg;
}

void *perl_fun_substr(void *str, void *offset, void *length)
{
  char *dst = NULL;
  int size = 0;
  switch(type_of(str)) {
  case STRING:
    if (length != NULL && !IS_UNDEF(length)) {
      size = to_native_int(length);
    } else {
      size = strlen(str) - to_native_int(offset);
    }
    dst = malloc((size+1)*sizeof(*dst));
    strncpy(dst, (char *) str + to_native_int(offset),
            size);
    dst[size] = '\0';
    break;
  case UNDEF:
    dst = malloc(sizeof(*dst));
    dst[0] = '\0';
    break;
  case NUMBER:
    return perl_fun_substr(to_native_string(str), offset, length);
  }
  return dst;
}
