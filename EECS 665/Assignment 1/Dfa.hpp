#ifndef DFA_H
#define DFA_H

#include <vector>
class Dfa {
  //Member fields
  //IMPLEMENT ME
private:


  //Member functions
 public:
  /**
   * Public constructor to construct the list
   */

  Dfa(std::vector<int> _nfaStates, std::vector<char> _inputs, int _stateNum);
  ~Dfa();

  /**
   * returns true if the list is empty or returns false
   */
  bool isEmpty();
  std::vector<int> nfaStates;
  std::vector<char> inputs;
  int stateNum;
  bool dfaMarked;
  int* stateChange;
};

#endif //DFA_H defined
