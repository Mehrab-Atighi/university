#include <stdio.h>
#include <math.h>

int main()
{
	FILE *file1 = fopen("input.txt" , "r");
	FILE *file2 = fopen("output.txt" , "w");
	int row = 0;
	int col = 0;
	fscanf(file1 , "%d %d" , &row , &col );
	
	fprintf(file2 ,"%d %d\n" , row , col );
	int a[row][col];
	float b;
	for(int i = 0 ; i < row ; i++)
	{
			for(int j = 0 ; j < col ; j++)
			{
				fscanf(file1 , "%d" , &a[i][j]);
				b = 1/a[i][j];
				fprintf(file2 , "%.2f " , b);
				printf("%d  %.2f " ,a[i][j], b);
			}
			fprintf(file2 , "\n");
	}

	fclose(file1);
	fclose(file2);
}
	

