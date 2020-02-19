#include <iostream>

using namespace std;

void ex1() {
  char test = 'a';
  char* p = &test;
  int array[10];
  /* Reference to array */
  int (&ref)[10] = array;
  string arrayS[2] = {"string1", "string2"};
  string* ptr = arrayS;
  char** p2 = &p;
  const int x = 2;
  const int* x_ptr = &x;
  int y = 2;
  int* const y_ptr = &y;
}

int main () {
  ex1();
}
