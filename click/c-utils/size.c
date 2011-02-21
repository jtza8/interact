#include <stdio.h>
#include <sys/time.h>

int main()
{
  printf("int size: %d\n", sizeof(int));
  printf("long size: %d\n", sizeof(long));
  printf("long long size: %d\n", sizeof(long long));
  printf("time_t size: %d\n", sizeof(time_t));
  printf("suseconds_t size: %d\n", sizeof(suseconds_t));
  return 0;
}
