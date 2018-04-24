// sample.c++: Code showing how to use ImageReader and Packed3DArray

#include "ImageReader.h"
#include "Packed3DArray.h"
#include <mpi.h>
#include <iostream>

void count(const cryph::Packed3DArray<unsigned char>* pa)
{
	// This simple example shows counting the number of instances of 138
	// in the provided image.
	int count = 0;
	// FOR EACH ROW OF THE IMAGE:
	for (int r=0 ; r<pa->getDim1() ; r++)
	{
		// FOR EACH COLUMN:
		for (int c=0 ; c<pa->getDim2() ; c++)
		{
			// FOR EACH CHANNEL (r, g, b):
			for (int rgb=0 ; rgb<pa->getDim3() ; rgb++)
				if (pa->getDataElement(r, c, rgb) == 138)
					count++;
		}
	}
	std::cout << "There were " << count << " instances of 138 in the image.\n";
}

void computeHistogram(cryph::Packed3DArray<unsigned char>* pa, float* red, float* green, float* blue) {

	//count the number of rgb values
	for(int i = 0; i < pa->getDim1(); i++) {
		for(int j = 0; j < pa->getDim2(); j++) {
			red[pa->getDataElement(i,j,0)]++;
			green[pa->getDataElement(i,j,1)]++;
			blue[pa->getDataElement(i,j,2)]++;
		}
	}

	//recalculate floating point percentage based on dimension of image
	for(int i = 0; i < 256; i++) {
		red[i] = red[i] / (pa->getDim1() * pa->getDim2());
		green[i] = green[i] / (pa->getDim1() * pa->getDim2());
		blue[i] = blue[i] / (pa->getDim1() * pa->getDim2());
	}
}

float absDif(float num1, float num2) {
	if(num1 < num2) {
		return (num2 - num1);
	} else if(num2 < num1) {
		return (num1 - num2);
	} else return 0.0f;
}

int getLowest(float* likeCounter, int communicatorSize) {
	int lowestIndex = 0;
	if(likeCounter[lowestIndex] == -1) {
		lowestIndex = 1;
	}
	for(int i = 0; i < communicatorSize; i++) {
		if(likeCounter[i] < likeCounter[lowestIndex] && likeCounter[i] != -1) {
			lowestIndex = i;
		}
	}
	return lowestIndex;
}

int compareSum(int rank, int communicatorSize, float* red, float* green, float* blue) {
	float* recvR = new float[256*communicatorSize];
	float* recvG = new float[256*communicatorSize];
	float* recvB = new float[256*communicatorSize];

	MPI_Allgather(red, 256, MPI_FLOAT, recvR, 256, MPI_FLOAT, MPI_COMM_WORLD);
	MPI_Allgather(green, 256, MPI_FLOAT, recvG, 256, MPI_FLOAT, MPI_COMM_WORLD);
	MPI_Allgather(blue, 256, MPI_FLOAT, recvB, 256, MPI_FLOAT, MPI_COMM_WORLD);

	float* likeCounter = new float[communicatorSize];
	//initialize like counter
	for(int i = 0; i < communicatorSize; i++) {
		likeCounter[i] = 0;
	}
	for(int i = 0; i < communicatorSize; i++) {
		if(rank != i) {
			for(int j = 0; j < 256; j++) {
				likeCounter[i] += absDif(red[j], recvR[j]);
				likeCounter[i] += absDif(green[j], recvG[j]);
				likeCounter[i] += absDif(blue[j], recvB[j]);
			}//end calc
		} else { //if the rank is the same
			likeCounter[i] = -1;
		}
	}//end rank iteration

	return getLowest(likeCounter, communicatorSize);
}

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	int rank, communicatorSize;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank); // unique ID for this process; 0<=rank<N where:
	MPI_Comm_size(MPI_COMM_WORLD, &communicatorSize); // N=communicatorSize (size of "world")
	//mpirun -n numFiles ./project2 j1.jpg j2.jpg j3.jpg
	int dimTag = 0;

	int imgTagR = 4;
	int imgTagG = 5;
	int imgTagB = 6;
	MPI_Request dimRequest;
	MPI_Request request;
	MPI_Status status;

	if (rank == 0)
	{
		//some code for the image processing
		for(int i = 1; i < argc; i++) {
			ImageReader* ir = ImageReader::create(argv[i]);
			count(ir->getInternalPacked3DArrayImage());
			if(ir == nullptr)
				std::cerr << "Could not open image file: " << argv[i] << '\n';
			else {
				cryph::Packed3DArray<unsigned char>* pa = ir->getInternalPacked3DArrayImage();
				int* bufferSize = new int[3];
				bufferSize[0] = pa->getDim1();
				bufferSize[1] = pa->getDim2();
				bufferSize[2] = pa->getDim3();

				MPI_Isend(bufferSize, 3, MPI_INT, i-1, dimTag, MPI_COMM_WORLD, &dimRequest);

				unsigned char* packedImg = pa->getModifiableData();
				std::cerr << " str length: " << strlen((char*)pa->getData());

				float* red = new float[256];
				float* green = new float[256];
				float* blue = new float[256];
				computeHistogram(pa, red, green, blue);

				MPI_Isend(red, 256, MPI_FLOAT, i-1, imgTagR, MPI_COMM_WORLD, &request);
				MPI_Isend(green, 256, MPI_FLOAT, i-1, imgTagG, MPI_COMM_WORLD, &request);
				MPI_Isend(blue, 256, MPI_FLOAT, i-1, imgTagB, MPI_COMM_WORLD, &request);
			}
		}
	}

	//all processes

	int* buffSize = new int[3];
	MPI_Recv(buffSize, 3, MPI_INT, 0, dimTag, MPI_COMM_WORLD, &status);
	unsigned char* packedImg;
	int size = buffSize[0]*buffSize[1]*buffSize[2];

	float* r = new float[256];
	float* g = new float[256];
	float* b = new float[256];

	MPI_Recv(r, 256, MPI_FLOAT, 0, imgTagR, MPI_COMM_WORLD, &status);
	MPI_Recv(g, 256, MPI_FLOAT, 0, imgTagG, MPI_COMM_WORLD, &status);
	MPI_Recv(b, 256, MPI_FLOAT, 0, imgTagB, MPI_COMM_WORLD, &status);

	int closestImageRank = compareSum(rank, communicatorSize, r, g, b);
	int* pClosestImgRank = &closestImageRank;
	int* bestImage = new int[communicatorSize];

	//normal gather finish before printing
	MPI_Gather(pClosestImgRank, 1, MPI_INT, bestImage, 1, MPI_INT, 0, MPI_COMM_WORLD);

	if(rank == 0) {
		std::cout << std::endl;
		for(int i = 0; i < communicatorSize; i++) {
			std::cout << "Process " << "[" << i << "] Favors: " << bestImage[i] << std::endl;
		}
	}

	MPI_Finalize();
	return 0;
}
