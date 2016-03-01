#include <stdio.h>
int main()
{
int i;
int j;
int k;
int s;

k=0;
s=0;

j=0;
while(j <= 10001)
 {
 i=0;
 while(i <= 10002)
  {
  k=1-k;
  s=s+k;
  i++;
  }
 s=s-1003;
 j++;
 };
printf("%d\n",s);
return 0;
}
