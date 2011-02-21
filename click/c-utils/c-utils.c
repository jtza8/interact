#include <stdio.h>
#include <sys/time.h>

long long get_ticks() {
  struct timeval time_val;
  gettimeofday(&time_val, NULL);
  return (long long)time_val.tv_sec * 1000000 + time_val.tv_usec;
}
