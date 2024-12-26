
#include <stdio.h>

int main()
{
  int min = 1 ;
  int max = 1000;
  int f = 0;
  printf("please enter youre maximum number here\n");
  scanf("%d" , &max);
  
  for( int i = min ; i<= max; i++)
  {
    int s = 0;
    int number = i;
    while( number != 0)
		{	
			int a = number%10;
			if(a == 1)
			{
				s++;
			}
			number = number/10;
		}
    if( s == 2)
    {
      f++;
    }}
    
  printf("youre range have %d numbers with two one .\n" , f);
  
}
