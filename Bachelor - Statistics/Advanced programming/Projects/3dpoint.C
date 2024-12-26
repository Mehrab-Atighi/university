#include <stdio.h>
#include <math.h>
struct point3d
{
	double x;
	double y;
	double z;
};

double distance3d (point3d p , point3d q )
{
		return sqrt( ((p.x - q.x) * (p.x - q.x)) + ((p.y - q.y) * (p.y - q.y)) + ((p.z - q.z) * (p.z - q.z)) );
}

int main()
{
	struct point3d a;
	struct point3d b;
	a.x = 0;
	a.y = 0;
	a.z = 0;
	b.x = 1;
	b.y = 1;
	b.z = 1;
	
	
	printf("the distance of this two points in R3 is equal to %f\n" , distance3d(a , b) );
	
}
// why i can not recive the coordinates with scanf function?
	//printf("please enter first number Coordinates (x,y,z)\n");
	//scanf("%f\n" , &a.x);
	//scanf("%f\n" , &a.y);
	//scanf("%f\n" , &a.z);
	//printf("please enter second number Coordinates (x,y,z)\n");
	//scanf("%f\n" , &b.x);
	//scanf("%f\n" , &b.y);
	//scanf("%f\n" , &b.z);
// By Mehrab Atighi.
