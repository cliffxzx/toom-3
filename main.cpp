#include <algorithm>
#include <chrono>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using namespace std;
#define MAXLENGTH 505
const int max_digit_length = 500;

typedef long long BigIntBase;
typedef vector<BigIntBase> BigIntDigits;

// ceil(numeric_limits<BigIntBase>::digits10 / 2.0) - 1;
static const int digit_base_len = 8;
// b
static const BigIntBase digit_base = 100000000;
class BigInt {
public:
  BigInt(int digits_capacity = 0) {
    negative = false;
    digits = BigIntDigits();
    digits.reserve(digits_capacity);
  }

  BigInt(const BigInt &num) {
    negative = num.negative;
    digits = BigIntDigits(num.digits);
  }

  BigInt(BigIntDigits _digits) {
    negative = false;
    digits = _digits;
  }

  BigInt(string s) {
    digits.reserve(ceil(max_digit_length / digit_base_len));
    negative = s[0] == '-';
    for (int pos = max(0, (int)s.size() - digit_base_len); !s.empty(); pos -= min(pos, digit_base_len)) {
      digits.push_back(stoll(s.substr(pos)));
      s.erase(pos);
    }
  }

  BigInt operator+(const BigInt &rhs) {
    BigInt lhs = *this;
    if (lhs.negative == rhs.negative) {
      BigInt result = plus(lhs.digits, rhs.digits);
      result.negative = lhs.negative;
      return result;
    }

    BigInt result;
    if (greater(lhs.digits, rhs.digits)) {
      result = minus(lhs.digits, rhs.digits);
      result.negative = lhs.negative;
    } else {
      result = minus(rhs.digits, lhs.digits);
      result.negative = rhs.negative;
    }

    return result;
  }

  BigInt operator-(const BigInt &rhs) {
    BigInt _rhs(rhs);
    _rhs.negative = !_rhs.negative;
    return *this + _rhs;
  }

  BigInt operator*(const BigInt &rhs) {
    const BigInt lhs = *this;
    if (lhs.digits.empty() || rhs.digits.empty()) {
      return BigInt();
    } else if (lhs.digits.size() == 1 && rhs.digits.size() == 1) {
      BigIntBase val = lhs.digits[0] * rhs.digits[0];
      BigIntDigits tmp;
      if (val < digit_base)
        tmp = {val};
      else
        tmp = {val % digit_base, val / digit_base};

      BigInt result(tmp);
      result.negative = lhs.negative ^ rhs.negative;
      return result;
    } else if (lhs.digits.size() == 1) {
      BigInt result = multiply(rhs, lhs.digits[0]);
      result.negative ^= lhs.negative;
      return result;
    } else if (rhs.digits.size() == 1) {
      BigInt result = multiply(lhs, rhs.digits[0]);
      result.negative ^= rhs.negative;
      return result;
    }

    BigInt result = toom3(lhs, rhs);
    result.negative = lhs.negative ^ rhs.negative;
    return result;
  }

  BigInt operator<<(const int n) {
    return shift_left(this->digits, n);
  }

  string to_string() {
    if (this->digits.empty())
      return "0";

    stringstream ss;
    if (this->negative)
      ss << "-";

    ss << std::to_string(this->digits.back());
    for (auto it = this->digits.rbegin() + 1; it != this->digits.rend(); ++it)
      ss << setw(digit_base_len) << setfill('0') << std::to_string(*it);

    return ss.str();
  }

  BigInt from_string(string s) {
    digits.clear();
    negative = s[0] == '-';
    for (int pos = max(0, (int)s.size() - digit_base_len); !s.empty(); pos -= min(pos, digit_base_len)) {
      digits.push_back(stoll(s.substr(pos)));
      s.erase(pos);
    }
    return *this;
  }

private:
  bool negative;
  BigIntDigits digits;

  BigInt toom3_slice_num(const BigInt &num, int n, int i) {
    BigIntDigits::const_iterator range[] = {num.digits.begin() + n * i, min(num.digits.end(), num.digits.begin() + (n + 1) * i)};
    if (range[0] >= num.digits.begin() && range[0] <= num.digits.end()) {
      range[1] = min(num.digits.end(), range[1]);
      return BigInt(BigIntDigits(range[0], range[1]));
    }

    return BigInt();
  }

  BigInt toom3(BigInt num1, BigInt num2) {
    int i = ceil(max(num1.digits.size() / 3.0, num2.digits.size() / 3.0));
    BigInt m0 = toom3_slice_num(num1, 0, i);
    BigInt m1 = toom3_slice_num(num1, 1, i);
    BigInt m2 = toom3_slice_num(num1, 2, i);
    BigInt n0 = toom3_slice_num(num2, 0, i);
    BigInt n1 = toom3_slice_num(num2, 1, i);
    BigInt n2 = toom3_slice_num(num2, 2, i);

    BigInt pt0 = m0 + m2;
    BigInt pp0 = m0;
    BigInt pp1 = pt0 + m1;
    BigInt pn1 = pt0 - m1;
    BigInt pn2 = multiply(pn1 + m2, 2) - m0;
    BigInt pin = m2;

    BigInt qt0 = n0 + n2;
    BigInt qp0 = n0;
    BigInt qp1 = qt0 + n1;
    BigInt qn1 = qt0 - n1;
    BigInt qn2 = multiply(qn1 + n2, 2) - n0;
    BigInt qin = n2;

    BigInt rp0 = pp0 * qp0;
    BigInt rp1 = pp1 * qp1;
    BigInt rn1 = pn1 * qn1;
    BigInt rn2 = pn2 * qn2;
    BigInt rin = pin * qin;

    BigInt r0 = rp0;
    BigInt r4 = rin;
    BigInt r3 = divide(rn2 - rp1, 3);
    BigInt r1 = divide(rp1 - rn1, 2);
    BigInt r2 = rn1 - rp0;
    r3 = divide(r2 - r3, 2) + multiply(rin, 2);
    r2 = r2 + r1 - r4;
    r1 = r1 - r3;

    BigInt result = r0;
    result = result + (r1 << i);
    result = result + (r2 << (i * 2));
    result = result + (r3 << (i * 3));
    result = result + (r4 << (i * 4));

    return result;
  }

  BigInt plus(const BigIntDigits &lhs, const BigIntDigits &rhs) {
    if (lhs.empty())
      return rhs;

    if (rhs.empty())
      return lhs;

    int max_length = max(lhs.size(), rhs.size());
    BigInt result(max_length);

    for (int w = 0; w < max_length; ++w) {
      BigIntBase num1 = 0, num2 = 0;
      if (lhs.size() > w)
        num1 = lhs[w];

      if (rhs.size() > w)
        num2 = rhs[w];

      result.digits.push_back(num1 + num2);
    }

    for (int w = 0; w < result.digits.size() - 1; ++w) {
      result.digits[w + 1] += result.digits[w] / digit_base;
      result.digits[w] %= digit_base;
    }

    if (result.digits.back() >= digit_base) {
      result.digits.push_back(result.digits.back() / digit_base);
      result.digits[result.digits.size() - 2] %= digit_base;
    }

    while (!result.digits.empty() && !result.digits.back())
      result.digits.pop_back();

    return result;
  }

  BigInt minus(const BigIntDigits &lhs, const BigIntDigits &rhs) {
    if (lhs.empty())
      return rhs;

    if (rhs.empty())
      return lhs;

    BigInt result(lhs.size() + 1);

    for (int w = 0; w < lhs.size(); ++w) {
      BigIntBase num1 = 0, num2 = 0;
      if (lhs.size() > w)
        num1 = lhs[w];

      if (rhs.size() > w)
        num2 = rhs[w];

      result.digits.push_back(num1 - num2);
    }

    for (int w = 0; w < result.digits.size() - 1; ++w)
      if (result.digits[w] < 0) {
        result.digits[w + 1] -= 1;
        result.digits[w] += digit_base;
      }

    while (!result.digits.empty() && !result.digits.back())
      result.digits.pop_back();

    return result;
  }

  BigInt divide(const BigInt &lhs, const int divisor) {
    BigIntDigits reminder(lhs.digits);
    BigIntDigits quotient(lhs.digits.capacity());

    for (int w = reminder.size() - 1; w >= 0; --w) {
      BigIntBase tmp = reminder[w] / divisor;
      quotient.insert(quotient.begin(), tmp);
      reminder[w - 1] += (reminder[w] % divisor) * digit_base;
    }

    while (!quotient.empty() && !quotient.back())
      quotient.pop_back();

    BigInt result(quotient);
    result.negative = lhs.negative;
    return result;
  }

  BigInt multiply(const BigInt &lhs, const int multiplier) {
    BigIntDigits nums(lhs.digits);

    for (int w = 0; w < nums.size(); ++w)
      nums[w] *= multiplier;

    for (int w = 0; w < nums.size(); ++w)
      if (nums[w] >= digit_base) {
        if (w + 1 == nums.size())
          nums.push_back(nums[w] / digit_base);
        else
          nums[w + 1] += nums[w] / digit_base;
        nums[w] %= digit_base;
      }

    while (!nums.empty() && !nums.back())
      nums.pop_back();

    BigInt result(nums);
    result.negative = lhs.negative;
    return result;
  }

  BigInt shift_left(const BigIntDigits &lhs, const int n) {
    BigInt result(lhs);
    for (int w = 0; w < n; ++w)
      result.digits.insert(result.digits.begin(), 0);

    while (!result.digits.empty() && !result.digits.back())
      result.digits.pop_back();

    return result;
  }

  bool greater(const BigIntDigits &lhs, const BigIntDigits &rhs) {
    if (lhs.size() == rhs.size()) {
      int w = lhs.size() - 1;
      while (w >= 0 && lhs[w] == rhs[w])
        --w;

      return w >= 0 && lhs[w] > rhs[w];
    } else
      return lhs.size() > rhs.size();
  }
};

/*
    目標 : 大數乘法 N^(log2 3) 盡量在200位內超車N^2，最好的情況是能在100位內超車

    O 使用宣告好的陣列，重複使用。
    X 禁止使用malloc
    ! 宣告Array的總長度需 < 5000

    input:
    10. 15. 20. 25. ...500位數

    步驟:
    1. 拿到兩數字，做前處理
     + 開始算時間
    2. for loop 200次(不斷重複call某一個計算func，回傳ans為string)
     + 結束時間
    3. 取平均

*/

// unsigned long long range = (0 ~ 18446744073709551615)
// 所以每格相乘在範圍內的最大值 = 18446744073709551615 ^ 0.5 = 4294967296
// 較方便的實作方法是 每格用 unsigned long long 存 9 位數

// 在更大的數字中 比較快的算法 Schönhage–Strassen algorithm
// 變數宣告
BigInt num1, num2;
string mul(string &s1, string &s2) {
  while (s1.back() == '\r' || s1.back() == '\n')
    s1.erase(s1.end() - 1);

  while (s2.back() == '\r' || s2.back() == '\n')
    s2.erase(s2.end() - 1);

  BigInt result = num1.from_string(s1) * num2.from_string(s2);
  return result.to_string();
}

int main() {
  string s1, s2, ans;
  int length;
  long long t;
  fstream cout("time2.txt", ios::out);

  // test 10 ~ 500位數
  for (int digit = 5; digit <= max_digit_length; digit += 5) {

    // 讀檔
    // read file
    ifstream in("inputs/" + to_string(digit) + ".in");
    getline(in, s1);
    getline(in, s2);

    // 資料前處理(string to int[ ] or string to char [ ] or ...)
    int testCnt = 1;

    // start timing
    auto start = chrono::high_resolution_clock::now();

    //200次乘法
    for (testCnt = 0; testCnt < 200; testCnt++) {
      // large number multiplication
      ans = mul(s1, s2);
    }

    // timing end
    auto end = chrono::high_resolution_clock::now();

    //average
    t = chrono::duration_cast<std::chrono::nanoseconds>(end - start).count() / testCnt;
    cout << t << endl;

    in.close();
  }

  return 0;
}