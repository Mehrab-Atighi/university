#include <stdio.h>

void print_matrix (int m , int n , int a[m][n])
{
	for (int i = 0 ; i < m ; i++)
	{
		for (int j = 0 ; j < n ; j++ )
		{
			printf("%d	" , a[i][j]);
		}
		printf("\n");
	}

}

#define ROW_1 (2)
#define COL_1 (3)
#define ROW_2 (3)
#define COL_2 (2)
#define ROW_3 (3)
#define COL_3 (3)

int main()
{
	int m1[ROW_1][COL_1] = {};
	int m2[ROW_2][COL_2] = {};
	int m3[ROW_3][COL_3] = {};

	
// now we want to asign value to each matrix.
	for( int i = 0 ; i < ROW_1 ; i++ )
	{
		for( int j = 0 ; j < COL_1 ; j++ )
		{
			m1[i][j] = i * j ;

		}
	}
for( int i = 0 ; i < ROW_2 ; i++ )
	{
		for( int j = 0 ; j < COL_2 ; j++ )
		{
			m2[i][j] = i * i ;	
		}
	}
for( int i = 0 ; i < ROW_3 ; i++ )
	{
		for( int j = 0 ; j < COL_3 ; j++ )
		{
			for( int k = 0 ; k < ROW_2 ; k++ )
			{
				m3[i][j]  += ((m1[i][k]) * (m2[k][i])) ;
			}	
		}
	}
	
// for print the output we have:
for ( int i = 0 ; i < ROW_3 ; i++ )
{
	for( int j = 0 ; j < COL_3 ; j++)
	{
		printf("%d\t" , m3[i][j]) ;
	}
	printf("\n");
}

}
print_matrix( int 2 , int 3 , int m1);

//by mehrab atighi.
