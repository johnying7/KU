#include <iostream>
#include <fstream>
#include <sstream>

#include <algorithm>
#include <utility>

#include <stack>
#include <vector>
#include <string>
#include <map>

using namespace std;

struct dfaState {
  bool marked;
  std::vector<int> states;
  std::map<char,int> moves;
};

typedef std::map<int, std::map<char, std::vector<int>>> typeNfaTable;
typedef std::map<int, dfaState> typeDfaTable;

int initialState, totalStates;
int numTransitions = 0; // holds the total number of input values / possible transitions
std::vector<int> finalStates;
std::vector<char> inputs;


typeNfaTable nfaTable;
typeDfaTable dfaTable;

void printVect(std::vector<int> vect){
  std::cout << "{";
  for(std::vector<int>::const_iterator i = vect.begin(); i != vect.end(); i++){
    if(i != vect.end()-1){
      std::cout << *i << ",";
    }
    else{
      std::cout << *i;
    }
  }
  std::cout << "} ";
}

int statesExist(std::vector<int> state, typeDfaTable DFATable){
  for(unsigned int i = 0; i < DFATable.size(); i++){
    if(DFATable[i].states == state){
      return i;
    }
  }
  return -1;
}

int unmarked(typeDfaTable DFATable){
  for(unsigned int i = 0; i < DFATable.size(); i++){
    if(!DFATable[i].marked){
      return i;
    }
  }
  return -1;
}

bool exists(std::vector<int> vect, int val){
  for(std::vector<int>::const_iterator i = vect.begin(); i != vect.end(); i++){
    if(*i == val){
      return true;
    }
  }
  return false;
}

//modify
std::vector<int> eclosure(std::vector<int> num, typeNfaTable NFATable){
  std::vector<int> closure;
  std::stack<int> stack;
  for(std::vector<int>::const_iterator i = num.begin(); i != num.end(); i++){
    stack.push(*i);
  }
  for(std::vector<int>::const_iterator i = num.begin(); i != num.end(); i++){
    closure.push_back(*i);
  }
  while( !(stack.empty()) ){
    int stackVal = stack.top();
    stack.pop();
    std::map<char, std::vector<int>> currentState = NFATable[stackVal];
    std::vector<int> EMoves = currentState['E'];

    for(std::vector<int>::const_iterator i = EMoves.begin(); i != EMoves.end(); i++){
      if( !(exists(closure, *i)) ){
        closure.push_back(*i);
        stack.push(*i);
      }
    }
  }
  std::sort(closure.begin(),closure.end());
  return closure;
}

std::vector<int> move(std::vector<int> num, char move, typeNfaTable NFATable){
  std::vector<int> temp;
  for(std::vector<int>::const_iterator i = num.begin(); i != num.end(); i++){
    std::vector<int> possibleStates = NFATable[*i][move];
    for(std::vector<int>::const_iterator j = possibleStates.begin(); j != possibleStates.end(); j++){
      if(!exists(temp, *j)){
        temp.push_back(*j);
      }
    }
  }
  std::sort(temp.begin(), temp.end());
  return temp;
}

dfaState newdfaState(bool marked, std::vector<int> vect){
  dfaState newDfa;
  std::map<char, int> base;
  newDfa.states = vect;
  newDfa.moves = base;
  newDfa.marked = marked;
  return newDfa;
}

std::vector<int> getFinalDfaStates(typeDfaTable DFATable, std::vector<int> finalStates){
  std::vector<int> states;
  for(unsigned int i = 0; i < DFATable.size(); i++){
    for(std::vector<int>::const_iterator j = finalStates.begin(); j != finalStates.end(); j++){
      if( exists(DFATable[i].states, *j) ){
        states.push_back(i);
      }
    }
  }
  return states;
}

void nfaToDfa(int initialState, std::vector<int> finalStates, typeNfaTable &NFATable, typeDfaTable &DFATable){
  int curStateNum = 0;
  std::vector<int> iStateVect;
  iStateVect.push_back(initialState);
  std::vector<int> closure = eclosure(iStateVect, NFATable);
  std::cout << "E-closure(IO) = ";
  printVect(closure);
  std::cout << " = " << curStateNum << "\n\n";
  dfaState initState = newdfaState(false, closure);
  DFATable[curStateNum] = initState;
  curStateNum++;

  while(unmarked(DFATable) >= 0){
    int k = unmarked(DFATable);
    DFATable[k].marked = true;
    std::cout << "\nMark " << k << std::endl;
    for(std::vector<char>::const_iterator w = inputs.begin(); w != inputs.end()-1; w++){
      std::vector<int> newMove = move(DFATable[k].states, *w, NFATable);
      std::vector<int> nMove = eclosure( newMove, NFATable);

      if(!newMove.empty()){
        printVect(DFATable[k].states);
        std::cout << "--" << *w << "--> ";
        printVect(newMove);
        std::cout << "\n";
        std::cout << "E-closure";
        printVect(newMove);
        std::cout << " = ";
        printVect(nMove);
        std::cout << " = ";
      }
      unsigned int temp = statesExist(nMove, DFATable);
      if(temp >= 0){
        std::cout << temp << "\n";
        DFATable[k].moves[*w] = temp;
      }
      else{
        if(!nMove.empty()){
          std::cout << curStateNum << "\n";
          dfaState newState = newdfaState(false, nMove);
          DFATable[curStateNum] = newState;
          DFATable[k].moves[*w] = curStateNum;
          curStateNum++;
        }
        else{
          DFATable[k].moves[*w] = -1;
        }
      }
    }
  }
  std::cout << "\n";
}

void printDfa(typeDfaTable DFATable){
  std::cout << "State      ";
  for(unsigned int i = 0; i < inputs.size()-1; i++){
    std::cout << inputs[i] << "        ";
  }
  std::cout << std::endl;
  for(unsigned int i = 0; i < DFATable.size(); i++){
    std::cout << i << "         ";
    for(unsigned int j = 0; j < inputs.size()-1; j++){
      std::cout << "{";
      if(DFATable[i].moves[j] != -1){
        std::cout << DFATable[i].moves[j];
      }
      std::cout << "}" << "       ";
    }
    std::cout << std::endl;
  }
}


// takes a list of states separated by comma's in {} and returns a vector array of them
// also puts the values in the stateList variable
std::vector<int> parseStates(std::string value) {
  int index;
  std::vector<int> stateList;
  if(value == "{}") return stateList;
  std::string tempVal;
  value.erase(value.begin());
  for(unsigned int i = 0; value.find(",") != std::string::npos; i++) {
    index = value.find(",");
    tempVal = value.substr(0,index);
    value.erase(0, index+1);
    stateList.push_back(stoi(tempVal));
  }
  value[value.find("}")] = 0;
  stateList.push_back(stoi(value));
  return stateList;
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
    initialState = stoi(value); //change xxxx
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
      std::map<char, std::vector<int>> stateChange;
      for(unsigned int j = 0; j < inputs.size(); j++) {
        std::vector<int> states;
        inputData >> value;
        states = parseStates(value);
        sort(states.begin(), states.end());
        stateChange[inputs[j]] = states;
      }
      //store values
      nfaTable[i] = stateChange;
    }
    //print dfa
    inputData.close();
}

int main(int argCount, char** args) {

  //read from the file and initialize
  initialize(argCount, args);

  //subset construction algo.
  nfaToDfa(initialState, finalStates, nfaTable, dfaTable);

  //printing out the Final DFA Table
  std::cout << "Initial State: {0}\n";
  std::cout << "Final State(s): ";
  printVect(getFinalDfaStates(dfaTable, finalStates));
  std::cout << "\n";
  printDfa(dfaTable);
  return 0;
}
