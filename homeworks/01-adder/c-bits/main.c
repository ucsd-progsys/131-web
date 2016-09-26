#include <stdio.h>

extern int our_code_starts_here() asm("our_code_starts_here");

int main(int argc, char** argv) {
  printf("%d\n", our_code_starts_here());
  return 0;
}
