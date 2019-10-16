#include <stdio.h>

int test(int a, int b)
{
  return a + b;
}

int main()
{
  int result;
  //result = test(1 2);
  //result = test(1, 2,);
  result = test(1, 2);
  printf("result is %d\n", result);
  return 0;
}
