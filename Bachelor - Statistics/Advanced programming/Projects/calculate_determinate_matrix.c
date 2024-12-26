#include <stdio.h>

void print_matrix (int nrow , int ncol , int a[nrow][ncol])
{
	for (int i = 0 ; i < nrow ; i++)
	{
		for (int j = 0 ; j < ncol ; j++ )
		{
			printf("%d	" , a[i][j]);
		}
		printf("\n");
	}

}  


double matrix_trace (int nrow,int ncol , int data[nrow][ncol])
{
	if(nrow == ncol)
	{
		int trace = 0;
		for ( int i = 0 ; i < nrow ; i++ ) 
		{
			trace += data[i][i];
			
		}
		printf("The matrix trace is equal to : %d." ,trace);
	}
	else
	{
		printf("For calculating trace of matrix you need to squred matrix");
	}
}



double matrix_det (int nrow,int ncol , int data[nrow][ncol])
{
	if(nrow == ncol)
	{
		int det  = 0;
		int main = 1 ;
		int secondary = 1 ;
		for ( int i = 0 ; i < nrow ; i++ ) 
		{
			main = main * data[i][i];
		}
		int i = 0;
		for ( int j = nrow-1 ; j >=0 ; j--)
		{
			
			secondary = secondary * data[i][j];
			i++;
		}
		det = main - secondary;
		printf("The matrix determinate is equal to : %d.\n" ,det);
	}
	else
	{
		printf("For calculating determinate of matrix you need to squred matrix");
	}
}



int matrix (int nrow,int ncol ,int data[nrow*ncol],int byrow)
{
	
	
	int matrix[nrow][ncol];
	int k=0;
	//if(len % nrow == 0 )
		//{
		if(byrow == 1)
		{
			for( int i = 0 ; i < nrow ; i++ )
				{
					for( int j = 0 ; j < ncol ; j++ )
					{
						
						matrix[i][j] = data[k];
						k++;
					}
				}
			
		print_matrix(nrow,ncol,matrix);
		
		}
		else if (byrow == 0)
		{
			for( int i = 0 ; i < ncol ; i++ )
				{
					for( int j = 0 ; j < nrow ; j++ )
					{
						
						matrix[j][i] = data[k];
						k++;
					}
				}
			
		print_matrix(nrow,ncol,matrix);	
		printf("So youre matrix dimantion, have %d rows (observation) and %d column (variable).\n",nrow , ncol);
		
		matrix_det(nrow , ncol , data);
		matrix_trace(nrow , ncol , data);	
		}
	//}
	//else
	//{
		//printf("warning : Please enter a true dimantion or chek youre data length!!");
	//}
}




int main()
{
	int a[10]= {1,2,3,4,5,6,7,8,9};
	matrix(3,3,a,0);

}
