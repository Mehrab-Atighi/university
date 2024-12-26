//#include <stdio.h>
//int main()
//{

	//float test1;
	//float test2;
	//float test3;
	//float test4;
	//float test5;
	//float sum;
	//float new_sum;
	//char name[50];
	//printf("please enter youre name here: \n");
	//scanf("%50s" , &name);
	//printf("please enter the first exam mark here: \n");
	//scanf("%f" , &test1);
	//printf("please enter the second exam mark here: \n");
	//scanf("%f" , &test2);
	//printf("please enter the third exam mark here: \n");
	//scanf("%f" , &test3);
	//printf("please enter the fourth exam mark here: \n");
	//scanf("%f" , &test4);
	//printf("please enter the fives exam mark here: \n");
	//scanf("%f" , &test5);
	//sum = test1 + test2 + test3 + test4 + test5	;
	//if (sum <= 50 )
	//{
		//printf("dear %s sum of youre mark from 50 is equal to %f \n" , name , sum);
	//printf("Now we want to change youre number limits to 0 to 20.\n");
	//new_sum = 0.4 * sum;
	//printf("dear %s sum of youre  new mark from 20 is equal to %lf \n" , name , new_sum);
	
	//}
	//else 
	//{
		//printf("please enter true numbers.(repeat this game:D)");
	//}
//}


// now we want to do this better:
#include <stdio.h>
int main()
{
	float max , min , new_max ;
	printf("please enter minimum of marks here: \n");
	scanf("%f" , &min);
	printf("please enter maximum of marks here: \n");
	scanf("%f" , &max);
	printf("please enter new maximum of marks here: \n");
	scanf("%f" , &new_max);
	int number_of_lessons = 5 ;
	printf("please enter number of lessons here: \n");
	scanf("%d" , &number_of_lessons);
	float mark , last_marks[number_of_lessons],  last_sum , new_sum ;
	for( int i = 0; i <number_of_lessons ; i++)
	{
		printf("please enter the exam %d  mark here: \n" , (i+1));
		scanf("%f" , &mark);
		last_marks[i] = mark;
		last_sum += last_marks[i];
	}
	new_sum = new_max * ((last_sum  - min )/(max - min )) ;
	printf("youre last number is equal to %lf and youre new number is equal to %f \n" , last_sum , new_sum );
}
