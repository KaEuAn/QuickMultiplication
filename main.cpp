#include <iostream>
#include <deque>
#include <vector>
#include <string>
#include <complex>

class BigInteger {
    static int base;
    std::vector <long long> number;
    size_t _size;
    mutable bool positive;

    inline bool isZero () const {
        return (_size == 0) || (_size == 1 && number[0] == 0);
    }

    void makeTransitionThroughDozens () {
        long long pre = 0;
        for (unsigned int i = 0; i < _size; ++i) {
            number[i] += pre;
            pre = number[i] / base;
            number[i] %= base;
        }
    }

    const BigInteger makeLeftSide (int i) const {
        BigInteger c(0);
        c._size = _size - i;
        c.positive = positive;
        if (c._size <= 0) {
            return c;
        }
        c.number[0] = number[i];
        for (long long j = i + 1; j < _size; ++j) {
            c.number.push_back(number[j]);
        }
        return c;
    }

    const BigInteger makeRightSide (int i) const {
        BigInteger c(0);
        c._size = i;
        c.positive = positive;
        c.number[0] = number[0];
        for (int j = 1; j < i; ++j) {
            c.number.push_back(number[j]);
        }
        c.PopZeroes();
        return c;
    }


public:

    BigInteger& operator << (int i) {
        _size += i;
        std::vector<long long> vec(_size, 0);
        for (int j = i; j < _size; ++j) {
            vec[j] = number[j-i];
        }
        number = vec;
        return *this;
    }

    BigInteger (int a): _size(0),  positive(a >= 0) {
        a = a >= 0 ? a : -1*a;
        while (a > 0) {
            number.push_back(a % base);
            a /= base;
            ++_size;
        }
        if (_size == 0) {
            number.push_back(0);
            ++_size;
        }
    }

    BigInteger (): _size(1), positive(true) {
        number.push_back(0);
    }

    BigInteger (const std::string& a): _size(0), positive(1) {
        if (a.length() > 0) {
            positive = (a[0] != '-');
            for (size_t i = a.length() - 1; i <= a.length(); ++_size) {
                if ( ! positive && i == 0)
                    break;
                int prom_num = 0, j = 0, x = 1;
                while (((i <= a.length() + 1 && positive) || (! positive && i > 0)) && j < 9) {
                    prom_num += ((short) a[i] - '0') * x;
                    x *= 10;
                    --i;
                    ++j;
                }
                number.push_back(prom_num);
            }
            PopZeroes();
        } else {
            _size = 1;
            number.push_back(0);
        }
    }



    BigInteger& operator += (const BigInteger& a)   {
        // plus
        if (positive == a.positive) {
            long long int more = 0;
            _size = (_size > a._size) ? _size + 1: a._size + 1;
            number.resize((signed int)_size, 0);
            for (size_t i = a._size - 1; i < a._size; --i) {
                number[i] += a.number[i] + more;
                more = number[i] / base;
                number[i] -= more * base;
                for (int j = 1; more; ++j) {
                    number[i + j] += more;
                    more  = number[i + j] / base;
                    number[i + j] -= more * base;
                }
            }
            PopZeroes();
        }
            // minus
        else {
            const BigInteger *p2 = &a;
            const BigInteger *p1 = &a;
            bool real_sign1 = positive;
            bool real_sign2 = a.positive;

            // change sign to compare only mod of numbers
            a.positive = true;
            positive = true;
            if (a > *this) {
                p2 = this;
            } else
                p1 = this;

            //making pointer on max and min element to choose a sign
            positive = real_sign1;
            a.positive = real_sign2;
            while (_size < a._size)
                PushOne();
            int per = 0;
            for (size_t i = 0; i < p2->_size || per == 1; ++i) {
                number[i] = (i < p2->_size)? p1->number[i] - (p2->number[i] + per) : p1->number[i] -(per);
                per = 0;
                if (number[i] < 0) {
                    number[i] += base;
                    per = 1;
                }
            }
            positive = p1->positive;
            PopZeroes();
        };
        return *this;
    }

    BigInteger& operator -= (const BigInteger& a) {
        if (this == &a) {
            *this = 0;
            return *this;
        }
        a.ChangeSign();
        *this += a;
        a.ChangeSign();
        return *this;
    }

    BigInteger operator *= (const BigInteger& a) {
        BigInteger c(0);
        c.positive = ! (positive ^ a.positive);

        //if one of them is zero
        if (! static_cast<bool> (a) || ! static_cast<bool> (*this)) {
            return *this = c;
        }

        c.number.resize(_size + a._size + 1, 0);
        for (unsigned long long i = _size-1; i <= _size; --i) {
            for (unsigned long long j = a._size-1; j <= a._size; --j) {
                c.number[i + j] += (number[i] * a.number[j]);      //multiolication
                long long per = c.number[i + j] / base;
                c.number[i + j] %= base;
                unsigned long long per_index = i + j + 1;
                while (per > 0) {                       // push over the ten
                    c.number[per_index] += per % base;
                    per = c.number[per_index] / base;
                    c.number[per_index] %= base;
                    ++per_index;
                }
            }
        }
        c.PopZeroes();
        *this = c;
        return *this;
    }

    BigInteger& operator /= (const BigInteger& a) {
        BigInteger q(0);
        q.number.resize(_size, 0);
        bool real_sign1 = positive;
        bool real_sign2 = a.positive;
        positive = true;
        a.positive = true;
        if (*this < a) {
            positive = real_sign1;
            a.positive = real_sign2;
            return *this = 0;
        }
        positive = real_sign1;
        a.positive = real_sign2;
        size_t n = a._size;
        PushOne();
        for (size_t j = _size - n - 1; j < _size - n; --j) {
            long long chast = (base * number[j + n] + number[j + n - 1]) / a.number[n - 1];
            long long r = (base * number[j + n] + number[j + n - 1]) % a.number[n - 1];
            if ((n - 2 <= n) && (chast == base || chast * a.number[n - 2] > base * r + number[j + n - 2])) {
                r += a.number[n - 1];
                --chast;
            }
            BigInteger q_um = a;
            q_um *= chast;
            int per = 0;
            for (size_t i = j; i < j + q_um._size; ++i) {
                //
                number[i] -= q_um.number[i-j] + per;
                per = 0;
                if (number[i] < 0) {
                    number[i] += 10;
                    per = 1;
                }
            }
            if (number[j + q_um._size] < 0) {
                --chast;
                long long more = 0;
                for (size_t i = j; i < q_um._size - 1; ++i) {
                    number[i] += q.number[i] + more;
                    more = number[i] / 10;
                    number[i] -= more * 10;
                    number[i + 1] += more;
                    more = 0;
                }
            }
            q.number[j] = chast;
        }
        PopZeroes();
        q.makeTransitionThroughDozens();
        q.PopZeroes();
        q.PopZeroes();
        *this = q;
        positive = ! (positive ^ a.positive);
        return *this;
    }

    BigInteger& operator %= (const BigInteger& a) {
        BigInteger c = *this;
        c /= a;
        c *= a;
        return *this -= c;
    }


    bool operator == (const BigInteger& a) const {
        if (isZero() && a.isZero()) return true;
        if (_size != a._size) return false;
        if (positive != a.positive) return false;
        for (size_t i = _size - 1; i <= _size; --i) {
            if (a.number[i] != number[i])
                return false;
        }
        return true;
    }

    bool operator != (const BigInteger& a) const {
        return ! (*this == a);
    }

    bool operator < (const BigInteger& a) const {
        if (*this == 0) {
            return a != 0 && a.positive;
        }else if (positive != a.positive) {
            return positive < a.positive;
        }
        bool answer = true;
        if (_size < a._size)
            answer = true;
        else if (_size > a._size)
            answer = false;
        else if (*this != a) {
            for (size_t i = _size-1; i <= _size; --i) {
                if (number[i] > a.number[i]) {
                    answer = false;
                    break;
                }
                else if (number[i] < a.number[i]) {
                    answer = true;
                    break;
                }
            }
        }
        answer = positive == answer;
        if (*this == a)
            answer = false;
        return answer;
    }

    bool operator > (const BigInteger& a) const {
        return ! (*this <= a);
    }

    bool operator >= (const BigInteger& a) const {
        return !(*this < a);
    }

    bool operator <= (const BigInteger& a) const {
        return (a == *this || *this < a);
    }


    long long getNumber (const int& i) const {
        return number[i];
    }
    size_t size () const {
        return _size;
    }

    const BigInteger KaratsubaMultiplication(BigInteger &a) {
        if (_size < a._size) {
            for (int i = _size; i < a._size; ++i) {
                number.push_back(0);
            }
            _size = a._size;
        }
        if (_size > a._size) {
            for (int i = a._size; i < _size; ++i) {
                a.number.push_back(0);
            }
            a._size = _size;
        }
        int m = std::max(_size, a._size);
        if (m < 36) {
            BigInteger X(*this);
            X *= a;
            return X;
        }
        m /= 2;
        BigInteger X_l, X_r, Y_l, Y_r, P1, P2, P3;
        X_l = makeLeftSide(m);
        X_r = makeRightSide(m);
        Y_l = a.makeLeftSide(m);
        Y_r = a.makeRightSide(m);
        P1 = X_l.KaratsubaMultiplication(Y_l);
        P2 = X_r.KaratsubaMultiplication(Y_r);
        X_l += X_r;
        Y_l += Y_r;
        P3 = X_l.KaratsubaMultiplication(Y_l);
        P3 -= P1;
        P3 -= P2;
        P3 << m;
        P1 << (m*2);
        P1 += P2;
        P1 += P3;
        return P1;
    }

    const BigInteger FurieMultiplication (BigInteger& a) const {
        std::vector <std::complex <double> > this_copy(number.begin(), number.end()), a_copy(a.number.begin(), a.number.end());
        int two_size = 1;
        while (two_size < _size && two_size < a._size)
            two_size <<= 1;
        this_copy.resize(two_size), a_copy.resize(two_size);
        bpf(this_copy, false);
        bpf(a_copy, false);
        for (size_t i = 0; i < two_size; ++i) {
            this_copy[i] *= a_copy[i];
        }
        bpf(this_copy, true);
        BigInteger answer;
        answer.number.resize
    }

    explicit operator bool () const {
        return !( *this == 0);
    }

private:
    void PopZeroes () {
        while (number.size() > 1 && number.back() == 0) {
            number.pop_back();
        }
        _size = number.size();
    }

    void PushOne () {
        number.push_back(0);
        ++_size;
    }

    void ChangeSign () const {
        positive = ! positive;
    }


};

int BigInteger::base = 1000000000;

std::ostream& operator << (std::ostream &stream, const BigInteger &a) {
    std::string str;
    if (a < 0)
        stream << '-';
    stream << a.getNumber(a.size() - 1);
    for (size_t i = a.size() - 2; i < a.size(); --i) {
        long long fer = a.getNumber(i);
        int sum = 9;
        if (fer == 0) {
            stream << "000000000";
            continue;
        }
        while (fer != 0) {
            fer /= 10;
            --sum;
        }
        for (int per = 0; per < sum; ++ per)
            stream << '0';
        stream << a.getNumber(i);
    }
    return stream;
}

std::istream& operator >> (std::istream &stream, BigInteger &a) {
    std::string str;
    stream >> str;
    BigInteger b(str);
    a = b;
    return stream;
}

const BigInteger operator + (const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    return c += b;
}
const BigInteger operator - (const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    return c -= b;
}
const BigInteger operator * (const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    return c *= b;
}
const BigInteger operator / (const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    return c /= b;
}
const BigInteger operator % (const BigInteger& a, const BigInteger& b) {
    BigInteger c = a;
    return c %= b;
}


int main() {
    std::string str;
    std::cin >> str;
    BigInteger a(str);
    std::cin >> str;
    BigInteger b(str);
    std::cout << a.KaratsubaMultiplication(b);
    return 0;
}