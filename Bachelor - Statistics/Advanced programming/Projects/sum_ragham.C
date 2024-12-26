
#include <stdio.h>

jame_argham (int s, int n )
{
	while(n >= 1)
	{	
			s += n%10;
			n /=10;
	}
	printf("jame arghame adade shoma : %d  mibashad" , s);	
	return s;
}
int main()
{
	int s = 0;
	int n;
	printf("please enter youre integer number\n");
	scanf("%d" ,&n);
	jame_argham(s , n);
}

