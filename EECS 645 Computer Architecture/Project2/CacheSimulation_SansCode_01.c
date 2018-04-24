//  J. Ying and L. Meadow
//  Created by Gary J. Minden on 9/24/15.
//  Copyright 2015 Gary J. Minden. All rights reserved.
//  Modified by: John Ying and Lyndon Meadow
//
//  Updates:
//
//      B40922 -- Added a loop with random indices
//
//
//	Description:
//			This program simulates a multi-way direct mapped cache.
//

#include <stdio.h>
#include <time.h>
#include <strings.h>
#include <stdlib.h>
#include <unistd.h>

#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>

//
//	Definitions of the cache
//
//	The cache size exponent and capacity in bytes
//
#define		CacheSize_Exp		15
#define		CacheSize_Nbr		( 1 << CacheSize_Exp )

//
//	Address size exponent
//
#define		AddressSize_Exp		32

//
//	The cache associativity
//
#define		CacheAssociativity_Exp	3
#define		CacheAssociativity	( 1 << CacheAssociativity_Exp )

//
//	The cache block size exponent, capacity in bytes, and mask
//
#define		BlockSize_Exp		6
#define		BlockSize_Nbr		( 1 << BlockSize_Exp )
#define		BlockSize_Mask		( BlockSize_Nbr - 1 )

//
//	The number of lines in the cache. A line can contain multiple blocks
//
#define		Lines_Exp			( (CacheSize_Exp) - (CacheAssociativity_Exp + BlockSize_Exp) )
#define		Lines_Nbr			( 1 << Lines_Exp )
#define		Lines_Mask			( Lines_Nbr - 1 )

//
//	Tag size exponent and mask
//
#define		Tag_Exp				( AddressSize_Exp - BlockSize_Exp - Lines_Exp )
#define		Tag_Nbr				( 1 << Tag_Exp )
#define		Tag_Mask			( Tag_Nbr - 1 )

//=============================================================================
//
//	Function to report defined values.
//
void ReportParameters ( char* theFilename ) {

	//
	// This group represents hexidecimal numbers of the variables
	//
	printf( "Filename: %s\n", theFilename );

	printf( "Cache Parameters: CacheSize_Exp: %08X; CacheSize_Nbr: %08X\n",
				CacheSize_Exp, CacheSize_Nbr );

	printf( "Address size: AddressSize_Exp: %08X\n", AddressSize_Exp );

	printf( "Cache Associativity: %08X\n", CacheAssociativity );

	printf( "Block Parameters: BlockSize_Exp: %08X; BlockSize_Nbr: %08X; BlockSize_Mask: %08X\n",
				BlockSize_Exp, BlockSize_Nbr, BlockSize_Mask );

	printf( "Line Parameters: Lines_Exp: %08X; Lines_Nbr: %08X; Lines_Mask: %08X\n",
				Lines_Exp, Lines_Nbr, Lines_Mask );

	printf( "Tag Parameters: Tag_Exp: %08X; Tag_Nbr: %08X; Tag_Mask: %08X\n",
				Tag_Exp, Tag_Nbr, Tag_Mask );

	//
	// This group represents decimal numbers of the variables
	//
	printf( "/-------------------/\nDecimal values\n/-------------------/\n" );
	printf( "Filename: %s\n", theFilename );

	printf( "Cache Parameters: CacheSize_Exp: %d; CacheSize_Nbr: %d\n",
				CacheSize_Exp, CacheSize_Nbr );

	printf( "Address size: AddressSize_Exp: %d\n", AddressSize_Exp );

	printf( "Cache Associativity: %d\n", CacheAssociativity );

	printf( "Block Parameters: BlockSize_Exp: %d; BlockSize_Nbr: %d; BlockSize_Mask: %d\n",
				BlockSize_Exp, BlockSize_Nbr, BlockSize_Mask );

	printf( "Line Parameters: Lines_Exp: %d; Lines_Nbr: %d; Lines_Mask: %d\n",
				Lines_Exp, Lines_Nbr, Lines_Mask );

	printf( "Tag Parameters: Tag_Exp: %d; Tag_Nbr: %d; Tag_Mask: %d\n",
				Tag_Exp, Tag_Nbr, Tag_Mask );

	printf("\n");
}

//=============================================================================
//
//	Main function
//
int main (int argc, const char * argv[]) {

	//
	//	Local variables
	//

	int Accesses_Nbr = 0;
	int CacheHit = 0;
	uint32_t Block; //value of the data
	uint32_t Line; //value of the line
	uint32_t Tag; //value of the tag

	//
	//	Allocate a BlockValid array.
	//
	unsigned int BlockValid[Lines_Nbr][CacheAssociativity];

	//
	//	Allocate a CacheTag array
	//
	unsigned int CacheTag[Lines_Nbr][CacheAssociativity];

	//
	//	Round Robin State
	//
	unsigned int RoundRobinState[Lines_Nbr]; //0 is invalid, 1 is valid

	//
	//	Process all files.
	//

	//read file stuff
	FILE *ptr_myfile;
	uint32_t Address = 0;
	char *files[] = {
		"AddressTrace_FirstIndex.bin",
		"AddressTrace_LastIndex.bin",
		"AddressTrace_RandomIndex.bin"
	};
	size_t numFiles = sizeof(files)/sizeof(char*); //finds the size of files
	for(int curFile = 0; curFile < numFiles; curFile++) {
		ptr_myfile=fopen(files[curFile],"rb");
		if (!ptr_myfile)
		{
			printf("Unable to open file!");
			return 1;
		}

		//
		//	Report cache parameters
		//
		ReportParameters( files[curFile] );

		//
		//	Open address trace file, reset counters, and process Accesses_Max addresses
		//
		Accesses_Nbr = 0;
		for(unsigned int i = 0; i < Lines_Nbr; i++) {
			RoundRobinState[i] = 0; //set all to invalid
		}

		//
		//	Flush cache
		//
		for(unsigned int i = 0; i < Lines_Nbr; i++) {
			for(unsigned int j = 0; j < CacheAssociativity; j++) {
				BlockValid[i][j] = 0;
			}
		}

		//
		//	Process the addresses in the trace
		//

		//while the fread pulls an integer from the file (until the end of file)
		while( fread(&Address,sizeof(Address),1,ptr_myfile) == 1) {
			//
			//	Check for end of file
			//
			Accesses_Nbr++;

			//
			//	Extract fields from address
			//
			Block = Address & BlockSize_Mask; //store the value into the block variable
			Address = (Address >> BlockSize_Exp); //remove the block from the address

			Line = Address & Lines_Mask; //gets the line number for the storage of the cache
			Address = (Address >> Lines_Exp); //remove the lines data from the address

			Tag = Address & Lines_Mask; //gets the tag for the storage of the cache
			Address = (Address >> Tag_Exp); //remove the tag data from the address

			//
			//	Check for cache hit
			//
			bool isCacheHit = false;
			for(unsigned int i = 0; i < CacheAssociativity; i++) {
				if( CacheTag[Line][i] == Tag && BlockValid[Line][i] == 1) {
					CacheHit++;
					isCacheHit = true;
				}
			}

			bool validBlockFound = false;
			if(!isCacheHit) {

				//
				//	Look for invalid (empty) block
				//
				for(unsigned int InvalidBlock_Idx = 0; InvalidBlock_Idx < CacheAssociativity; InvalidBlock_Idx++) {
					if( BlockValid[Line][InvalidBlock_Idx] == 0 ) { //store the stuff
						CacheTag[Line][InvalidBlock_Idx] = Tag;
						BlockValid[Line][InvalidBlock_Idx] = 1; //set the block to modified
						validBlockFound = true;
						break; //exit the search for invalid empty block
					}
				}

				//
				//	If no invalid block, then we must kick out a block
				//		For now, we'll use a common round robin state
				//
				if(!validBlockFound) {
					if( RoundRobinState[Line] + 1 == CacheAssociativity ) {
						RoundRobinState[Line] = 0;
					} else {
						RoundRobinState[Line]++;
					}
				}
			} //end of if statement "isCacheHit"

			uint32_t RoundRobinNextReplacement = RoundRobinState[Line]; //for printing

		}
		fclose(ptr_myfile);

		//
		//	Report cache performance
		//
		float hitRatio = 1.0*CacheHit/Accesses_Nbr;
		printf( "    Cache Performance: Accesses: %d; CacheHits: %d; Ratio: %.2f\n\n",
			Accesses_Nbr, CacheHit, hitRatio );
	}

	//
	//	Return
	//
	return( 1 );

}
