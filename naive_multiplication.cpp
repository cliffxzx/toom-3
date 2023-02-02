#include <chrono>
#include <fstream>
#include <iostream>
#include <string>
#define MAX_LENGTH 500

using namespace std;
int array1[MAX_LENGTH];
int array2[MAX_LENGTH];
int product[MAX_LENGTH + MAX_LENGTH];
long long t, totalT;

string ans_string(int len) {
  int i = len * 2 - 1;
  string ans;

  if (product[i] == 0)
    i--;

  for (; i >= 0; i--)
    ans += to_string(product[i]);
  return ans;
}

string mul(int len) {

  for (int i = 0; i < len; i++) //init
  {
    product[i] = 0;
    product[i + len] = 0;
  }
  for (int i = 0; i < len; i++) //cal
  {
    for (int j = 0; j < len; j++) {
      if (array2[j] == 0)
        continue;
      product[i + j] += array1[i] * array2[j];
    }
  }
  for (int i = 0; i < 2 * len - 1; i++) //carry
  {
    product[i + 1] += product[i] / 10;
    product[i] %= 10;
  }

  return ans_string(len);
}

int main() {
  string s1, s2, ans;

  for (int digit = 10; digit <= 500; digit += 5) // test 10 ~ 500位數
  {
    ifstream in("inputs/" + to_string(digit) + ".in"); // read file
    getline(in, s1);
    getline(in, s2);

    for (int i = 0; i < digit; i++) //string to int array
    {
      array1[i] = s1[digit - i - 1] - '0';
      array2[i] = s2[digit - i - 1] - '0';
    }

    int testCnt;
    auto start = chrono::high_resolution_clock::now(); // start timing
    for (testCnt = 0; testCnt < 200; testCnt++)        //100次乘法
    {
      ans = mul(digit);
      // cout << ans << endl;
    }
    auto end = chrono::high_resolution_clock::now();                                    // timing end
    t = chrono::duration_cast<std::chrono::nanoseconds>(end - start).count() / testCnt; //average
    cout << t << endl;
    in.close();
  }

  return 0;
}
