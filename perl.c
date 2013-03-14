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

/* TODO: perl_times */
/* TODO: perl_divide */
/* TODO: perl_concat */

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

/* TODO: perl_different */
/* TODO: perl_greater */
/* TODO: perl_lower */
/* TODO: perl_greater_equals */
/* TODO: perl_lower_equals */

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

/* TODO: perl_str_different */
/* TODO: perl_str_greater */
/* TODO: perl_str_lower */
/* TODO: perl_str_greater_equals */
/* TODO: perl_str_lower_equals */

/*********** Standard functions *************************/
void *defined(void *arg)
{
  return (void *)!IS_UNDEF(arg);
}

void *print(void *arg)
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

void *length(void *arg)
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

void *scalar(void *arg)
{
  /* TODO: what does scalar do? */
  return arg;
}

void *substr(void *str, void *offset, void *length)
{
  char *dst = NULL;
  int size = 0;
  switch(type_of(str)) {
  case STRING:
    if (length != NULL) {
      size = to_native_int(length);
    } else {
      size = strlen(str) - to_native_int(offset);
    }
    dst = malloc(size*sizeof(*dst));
    strncpy(dst, (char *) str + to_native_int(offset),
            size);
  case UNDEF:
    dst = malloc(sizeof(*dst));
    dst[0] = '\0';
  case NUMBER:
    return substr(to_native_string(str), offset, length);
  }
  return dst;
}
