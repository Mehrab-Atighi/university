#include <stdio.h>
#define COLINEAR (0);
#define CLOCKWISE (1);
#define COUNTERCLOCkWISE (-1);

typedef struct {int x; int y;} point2d;

int flag (point2d p1, point2d p2, point2d p3)
{
	int P = ((p2.x - p1.x) * (p3.y - p1.y)) - ((p2.y - p1.y) * (p3.x - p1.x));
	if( P == 0)
	{
		return COLINEAR;
	}
	if( P > 0)
	{
		return CLOCKWISE;
	}
	if( P > 0)
	{
		return COUNTERCLOCkWISE;
	}
}
int main()
{
	point2d A = {1,1};
	point2d B = {-1,1};
	point2d C = {-1,-1};
	point2d D = {1,-1};
	point2d point = {0,0};
	
	if( (flag(A , B , point) == 1)&& (flag(B , C , point) == 1)&& (flag(C , D , point) == 1)&& (flag(D , A , point) == 1))
	{
		printf("the point is in the mostatil :) " );
	}
	else
	{
		printf("the point is not in the mostatil ");
	}
}

