/**
  Name: John Ying
  Course: EECS 665 Compilers
  Assignment 1
**/
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

// #include "util.hpp"
// #include "list.hpp"
#include "State.hpp"
#include "converter.hpp"

using namespace std;

void printMenu();
void initialize(int argCount, char** args);
std::vector<int> parseStates(std::string states);
// void cleanUp(DoublyLinkedList* myList);
/**
 * This is the main entry point for the application
 * if you want to use your own custom datafile you'd have to pass that
 * as an argument to the function.
 * i.e. ./main mytest.txt
 */

int initialState, totalStates;
int numTransitions = 0; // holds the total number of input values / possible transitions
std::vector<int> finalStates;
std::vector<char> inputs;
std::vector<State> listOfStates;

int main(int argCount, char** args){

  //read from the file and initialize the doubly link list
  initialize(argCount, args);

  return 0;
}

/**
 * Initializes the data structures and program environment
 */
void initialize(int argCount, char** args){
    fstream inputData;
    if(argCount < 2){
      cout << "No input file given, using default data.txt" << endl;
      inputData.open("data.txt", ifstream::in);
    } else {
      cout << "Using data from " << args[1] << endl;
      inputData.open(args[1], ifstream::in);
    }

    //----
    //begin reading data...
    //----
    std::string value;
    inputData >> value >> value >> value;
    value.erase(value.begin());
    value[value.find("}")] = 0;
    initialState = stoi(value);
    cout << "initial state: " << initialState << std::endl;

    //assumes at least one final state
    inputData >> value >> value >> value;

    //read the final states and put them into finalStates variable
    finalStates = parseStates(value);

    cout << "final states: ";
    for(unsigned int i = 0; i < finalStates.size(); i++) {
      cout << finalStates[i] << " ";
    }
    cout << std::endl;

    //read the total number of states
    inputData >> value >> value >> value;
    totalStates = stoi(value);
    cout << "total states: " << totalStates << std::endl;

    //read all of the possible input values
    inputData >> value;
    cout << "possible input values: ";
    while(value != "E") {
      inputData >> value;
      inputs.push_back(value[0]);
      cout << value[0] << " ";
      numTransitions++;
    }
    cout << std::endl;

    //read the states
    for(int i = 0; i < totalStates; i++) {
      inputData >> value;
      std::vector<std::vector<int>> stateChange;
      for(int j = 0; j < numTransitions; j++) {
        inputData >> value;
        stateChange.push_back(parseStates(value));
      }
      listOfStates.push_back(State(i+1,inputs,stateChange));
    }

    Converter myConverter = Converter(listOfStates, inputs, finalStates.size(), finalStates, numTransitions, initialState, totalStates);
    myConverter.printDfa();
    inputData.close();
}

// takes a list of states separated by comma's in {} and returns a vector array of them
// also puts the values in the stateList variable
std::vector<int> parseStates(std::string value) {
  int index;
  std::vector<int> stateList;
  if(value == "{}") return stateList;
  std::string tempVal;
  value.erase(value.begin());
  for(int i = 0; value.find(",") != std::string::npos; i++) {
    index = value.find(",");
    tempVal = value.substr(0,index);
    value.erase(0, index+1);
    stateList.push_back(stoi(tempVal));
  }
  value[value.find("}")] = 0;
  stateList.push_back(stoi(value));
  return stateList;
}
// void cleanUp(DoublyLinkedList* myList){
//   delete myList;
// }
