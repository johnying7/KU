#include <thread>
#include <mutex>
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include "Barrier.h"

Barrier timeStep;
Barrier startTimeStep; //secures uniform unlocking without trying to lock before next time step
std::mutex coutMutex;
std::mutex** routeMutex;
int finishedRoutes;
std::vector <int> stepCompletion;

int gHigh(int x, int y){
	if (x < y) return y;
	else if (y < x) return x;
	else {
		std::cout << "Error: gHigh same comparison" << std::endl;
		return -1;
	}
}

int gLow(int x, int y){
	if (x < y) return x;
	else if (y < x) return y;
	else {
		std::cout << "Error: gLow same comparison" << std::endl;
		return -1;
	}
}

bool lock(int x, int y) {
	return routeMutex[gLow(x,y)][gHigh(x,y)].try_lock();
}

void unlock(int x, int y) {
	return routeMutex[gLow(x,y)][gHigh(x,y)].unlock();
}

void train(int assignment, int nTrains, std::vector<int> route)
{
	int iTimeStep = 0; //used to keep track of time step completion
	char trainID = (char)(65+assignment); //marks the train by number to char ascii comparison
	unsigned int i = 0; //index marking train's current station

	startTimeStep.barrier(nTrains); //preps all threads for launch

	while(i < route.size()-1) {

		if(route[i] == route[i+1]) {
			coutMutex.lock();
			std::cout << "At time step: " << iTimeStep
				<< " train " << trainID
				<< " is scheduled to stay at station " << route[i] << "\n";
			coutMutex.unlock();

			i++;

			timeStep.barrier(nTrains);
			startTimeStep.barrier(nTrains);

		} else if(lock(route[i], route[i+1])) {

			coutMutex.lock();
			std::cout << "At time step: " << iTimeStep
				<< " train " << trainID
				<< " is going from station " << route[i]
				<< " to station " << route[i+1] << "\n";
			coutMutex.unlock();

			if(i == route.size()-2) {
				unlock(route[i], route[i+1]);
				finishedRoutes++;
			} else {
				timeStep.barrier(nTrains);
				unlock(route[i], route[i+1]);
				startTimeStep.barrier(nTrains);
			}
			i++; //move successful

		} else {
			coutMutex.lock();
			std::cout << "At time step: " << iTimeStep
				<< " train " << trainID
				<< " must stay at station " << route[i] << "\n";
			coutMutex.unlock();

			timeStep.barrier(nTrains);
			startTimeStep.barrier(nTrains);
		}
		iTimeStep++;
	}

	while(finishedRoutes <= nTrains) {
		timeStep.barrier(nTrains);
		if(finishedRoutes == nTrains) {
			break;
		}
		startTimeStep.barrier(nTrains);
	}

	stepCompletion[assignment] = iTimeStep-1;
}

int main(int argc, char* argv[])
{
	//check the file for validity/propriety
	if (argc < 1) {
		std::cout << "need a data file..." << std::endl;
		return 0;
	}
	std::ifstream fileIn(argv[1]);
	if (!fileIn.good()) {
		std::cout << "Couldn't open file: " << argv[1] << std::endl;
		return 0;
	}

  int nTrains;
  int nStations;
	finishedRoutes = 0;

  fileIn >> nTrains >> nStations;
  std::cout << "nTrains: " << nTrains << "\n";
  std::cout << "nStations: " << nStations << "\n";

	//assign default input variables
	stepCompletion.resize(nTrains);

	std::vector <std::vector<int>> trainInfo(nTrains);
	routeMutex = new std::mutex*[nStations];
	for(int i = 0; i < nStations; i++) {
		routeMutex[i] = new std::mutex[nStations];
	}

	std::string tString;
  std::getline(fileIn, tString); //break to get next line (eating last space)

	//read each train route and input into 2d vector array
  for(int i = 0; i < nTrains; i++) {
		int tempNum; //train's current station
    std::getline(fileIn, tString); //grab single train route
		std::stringstream tempStream(tString); //put route into a stream
		int routeLength;
		tempStream >> routeLength;
		for(int j = 0; j < routeLength; j++){ //extract a station from the stream while there are stations left
			tempStream >> tempNum;
			trainInfo[i].push_back(tempNum); //add that station to the array
		}
  }

	//prints the array to console (to represent file input to 2D array memory)
	for(int i = 0; i < nTrains; i++) {
		std::cout << trainInfo[i].size() << " ";
		for(unsigned int j = 0; j < trainInfo[i].size(); j++) {
			std::cout << trainInfo[i][j] << " ";
		}
		std::cout << "\n";
	}

	//begins thread application
	std::thread** t = new std::thread*[nTrains];
	for(int i = 0; i < nTrains; i++) {
		t[i] = new std::thread(train, i, nTrains, trainInfo[i]);
	}

	for(int i = 0; i < nTrains; i++) {
		t[i]->join();
	}
	std::cout << "Simulation complete.\n\n";

	for(int i = 0; i < nTrains; i++) {
		char trainID = (char)(65+i);
		std::cout << "Train " << trainID << " completed its route at time step " << stepCompletion[i] << std::endl;
	}
  fileIn.close();
	return 0;
}
