#include <stdio.h> 

void main ()
{

	FILE *fptr , *fptr1 , *fptr2 ;
	int num ;
	int i ;
	fptr = fopen ("IdentifyControllerDataStructure.txt" , "w") ;
	fptr1 = fopen ("IdentifyNamespaceDataStructure.txt" , "w") ;
	fptr2 = fopen ("LBARangeDataStructure.txt" , "w") ;
	
	if ( fptr == NULL ) 
	{
		printf(" FILE does not exist " ) ;
		return ;
	}

	for ( i = 0 ; i < 1024 ; i++ ) 
	{
		fprintf(fptr , "%x\n " , 0 ) ;
		fprintf(fptr1 , "%x\n " , 0 ) ;
		fprintf(fptr2 , "%x\n " , 0 ) ;
	}

	fclose(fptr) ;
}

